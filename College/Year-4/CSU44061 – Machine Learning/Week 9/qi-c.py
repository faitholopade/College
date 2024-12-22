import torch
import torch.nn as nn
from torch.nn import functional as F

# Hyperparameters
batch_size = 32
block_size = 128
max_iters = 1000  # Limit to 1000 iterations
eval_interval = 200
learning_rate = 3e-4
device = 'cuda' if torch.cuda.is_available() else 'cpu'
eval_iters = 200
dropout = 0.1

# Model configurations for downsizing
configs = [
    {"n_embd": 256, "n_head": 4, "n_layer": 4},  # Original configuration
    {"n_embd": 128, "n_head": 2, "n_layer": 2},  # Smaller embedding and fewer layers
    {"n_embd": 64, "n_head": 1, "n_layer": 2},   # Even smaller embedding and fewer heads
]

# Load data
with open('/kaggle/input/input-childspeech-trainingset-txt/input_childSpeech_trainingSet.txt', 'r', encoding='utf-8') as f:
    text = f.read()

chars = sorted(list(set(text)))
vocab_size = len(chars)
stoi = {ch: i for i, ch in enumerate(chars)}
itos = {i: ch for i, ch in enumerate(chars)}
encode = lambda s: [stoi[c] for c in s]
decode = lambda l: ''.join([itos[i] for i in l])

data = torch.tensor(encode(text), dtype=torch.long)
n = int(0.9 * len(data))
train_data = data[:n]
val_data = data[n:]

def get_batch(split):
    data = train_data if split == 'train' else val_data
    ix = torch.randint(len(data) - block_size, (batch_size,))
    x = torch.stack([data[i:i + block_size] for i in ix])
    y = torch.stack([data[i + 1:i + block_size + 1] for i in ix])
    return x.to(device), y.to(device)

@torch.no_grad()
def estimate_loss(model):
    losses = {"train": [], "val": []}
    model.eval()
    for split in ['train', 'val']:
        loss_sum = 0
        for _ in range(eval_iters):
            x, y = get_batch(split)
            _, loss = model(x, y)
            loss_sum += loss.item()
        losses[split] = loss_sum / eval_iters
    model.train()
    return losses

class TransformerModel(nn.Module):
    def __init__(self, config):
        super().__init__()
        self.token_embedding = nn.Embedding(vocab_size, config["n_embd"])
        self.position_embedding = nn.Embedding(block_size, config["n_embd"])
        self.blocks = nn.Sequential(*[Block(config["n_embd"], config["n_head"]) for _ in range(config["n_layer"])])
        self.ln_f = nn.LayerNorm(config["n_embd"])
        self.head = nn.Linear(config["n_embd"], vocab_size)
        self.apply(self._init_weights)

    def _init_weights(self, module):
        if isinstance(module, nn.Linear) or isinstance(module, nn.Embedding):
            nn.init.normal_(module.weight, mean=0.0, std=0.02)
            if hasattr(module, "bias") and module.bias is not None:
                nn.init.zeros_(module.bias)

    def forward(self, idx, targets=None):
        B, T = idx.shape
        token_emb = self.token_embedding(idx)
        pos_emb = self.position_embedding(torch.arange(T, device=device))
        x = token_emb + pos_emb
        x = self.blocks(x)
        x = self.ln_f(x)
        logits = self.head(x)
        loss = None
        if targets is not None:
            loss = F.cross_entropy(logits.view(-1, logits.size(-1)), targets.view(-1))
        return logits, loss

    def generate(self, idx, max_new_tokens):
        for _ in range(max_new_tokens):
            idx_cond = idx[:, -block_size:]  # Ensure context fits within the block size
            logits, _ = self(idx_cond)
            logits = logits[:, -1, :]  # Focus on the last time step
            probs = F.softmax(logits, dim=-1)
            idx_next = torch.multinomial(probs, num_samples=1)
            idx = torch.cat((idx, idx_next), dim=1)
        return idx


class Block(nn.Module):
    def __init__(self, n_embd, n_head):
        super().__init__()
        head_size = n_embd // n_head
        self.sa = MultiHeadAttention(n_head, head_size, n_embd)
        self.ffwd = FeedForward(n_embd)
        self.ln1 = nn.LayerNorm(n_embd)
        self.ln2 = nn.LayerNorm(n_embd)

    def forward(self, x):
        x = x + self.sa(self.ln1(x))
        x = x + self.ffwd(self.ln2(x))
        return x

class MultiHeadAttention(nn.Module):
    def __init__(self, n_head, head_size, n_embd):
        super().__init__()
        self.heads = nn.ModuleList([Head(head_size, n_embd) for _ in range(n_head)])
        self.proj = nn.Linear(n_embd, n_embd)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x):
        out = torch.cat([h(x) for h in self.heads], dim=-1)
        return self.dropout(self.proj(out))

class Head(nn.Module):
    def __init__(self, head_size, n_embd):  # Added n_embd here
        super().__init__()
        self.key = nn.Linear(n_embd, head_size, bias=False)
        self.query = nn.Linear(n_embd, head_size, bias=False)
        self.value = nn.Linear(n_embd, head_size, bias=False)
        self.register_buffer("tril", torch.tril(torch.ones(block_size, block_size)))

    def forward(self, x):
        B, T, C = x.shape
        k = self.key(x)
        q = self.query(x)
        # Compute attention scores (T x T matrix per batch)
        wei = (q @ k.transpose(-2, -1)) * (C ** -0.5)
        wei = wei.masked_fill(self.tril[:T, :T].to(wei.device) == 0, float('-inf'))
        # Apply softmax
        wei = F.softmax(wei, dim=-1)
        # Weighted sum of values
        v = self.value(x)
        out = wei @ v
        return out


class FeedForward(nn.Module):
    def __init__(self, n_embd):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(n_embd, 4 * n_embd),
            nn.ReLU(),
            nn.Linear(4 * n_embd, n_embd),
            nn.Dropout(dropout),
        )

    def forward(self, x):
        return self.net(x)

# Add text generation and qualitative assessment
def generate_sample(model, context, max_new_tokens=100, sample_count=1):
    model.eval()
    for i in range(sample_count):
        generated_idx = model.generate(context, max_new_tokens=max_new_tokens)
        generated_text = decode(generated_idx[0].tolist())
        print(f"Sample {i + 1}:\n{generated_text}\n")
    model.train()

# Initialise context for generation (start with an empty sequence or seed text)
context = torch.zeros((1, 1), dtype=torch.long, device=device)

# Train and evaluate for each configuration
for config in configs:
    print(f"Training with config: {config}")
    model = TransformerModel(config).to(device)
    optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate)

    for iter in range(max_iters):
        if iter % eval_interval == 0 or iter == max_iters - 1:
            losses = estimate_loss(model)
            print(f"Iter {iter}: Train Loss {losses['train']:.4f}, Val Loss {losses['val']:.4f}")
        
        xb, yb = get_batch('train')
        _, loss = model(xb, yb)
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

    # Generate samples for qualitative assessment
    print("\nQualitative Assessment of Generated Output:")
    generate_sample(model, context)

    # Log final losses for discussion
    losses = estimate_loss(model)
    print(f"Final Train Loss: {losses['train']:.4f}, Final Val Loss: {losses['val']:.4f}")
    print("-" * 80)

print("Training complete for all configurations.")

