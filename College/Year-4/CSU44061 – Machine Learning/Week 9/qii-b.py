import torch
import torch.nn as nn
from torch.nn import functional as F

# Hyperparameters
batch_size = 32
block_size = 128
learning_rate = 3e-4
max_iters = 5000
eval_interval = 500
eval_iters = 200
dropout = 0.1
device = 'cuda' if torch.cuda.is_available() else 'cpu'

# Model configuration
config = {"n_embd": 256, "n_head": 4, "n_layer": 4}

# Load and preprocess the training data
def load_data(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        text = f.read()
    chars = sorted(list(set(text)))
    stoi = {ch: i for i, ch in enumerate(chars)}
    itos = {i: ch for ch, i in stoi.items()}
    encode = lambda s: [stoi[c] for c in s]
    decode = lambda l: ''.join([itos[i] for i in l])
    data = torch.tensor(encode(text), dtype=torch.long)
    return data, stoi, itos, len(chars)

train_data, stoi, itos, vocab_size = load_data('/kaggle/input/input-shakespeare-txt/input_shakespeare.txt')

# Split into training and validation sets
n = int(0.9 * len(train_data))
train_split = train_data[:n]
val_split = train_data[n:]

def get_batch(split):
    data = train_split if split == 'train' else val_split
    ix = torch.randint(len(data) - block_size, (batch_size,))
    x = torch.stack([data[i:i + block_size] for i in ix])
    y = torch.stack([data[i + 1:i + block_size + 1] for i in ix])
    return x.to(device), y.to(device)

@torch.no_grad()
def evaluate_loss(model):
    model.eval()
    losses = {"train": [], "val": []}
    for split in ['train', 'val']:
        loss_sum = 0
        for _ in range(eval_iters):
            x, y = get_batch(split)
            _, loss = model(x, y)
            loss_sum += loss.item()
        losses[split] = loss_sum / eval_iters
    model.train()
    return losses

# Define the Transformer model
class Transformer(nn.Module):
    def __init__(self, config):
        super().__init__()
        self.token_embedding = nn.Embedding(vocab_size, config["n_embd"])
        self.position_embedding = nn.Embedding(block_size, config["n_embd"])
        self.layers = nn.Sequential(*[TransformerBlock(config) for _ in range(config["n_layer"])])
        self.ln_f = nn.LayerNorm(config["n_embd"])
        self.head = nn.Linear(config["n_embd"], vocab_size)
        self.apply(self._init_weights)
        
    def _init_weights(self, module):
        if isinstance(module, nn.Linear):
            nn.init.normal_(module.weight, mean=0.0, std=0.02)
            if module.bias is not None:
                nn.init.zeros_(module.bias)
        elif isinstance(module, nn.Embedding):
            nn.init.normal_(module.weight, mean=0.0, std=0.02)

    def forward(self, idx, targets=None):
        B, T = idx.size()
        token_emb = self.token_embedding(idx)
        pos_emb = self.position_embedding(torch.arange(T, device=device))
        x = token_emb + pos_emb
        x = self.layers(x)
        x = self.ln_f(x)
        logits = self.head(x)
        loss = None
        if targets is not None:
            loss = F.cross_entropy(logits.view(-1, logits.size(-1)), targets.view(-1))
        return logits, loss

class TransformerBlock(nn.Module):
    def __init__(self, config):
        super().__init__()
        self.attn = MultiHeadAttention(config["n_head"], config["n_embd"] // config["n_head"])
        self.ff = FeedForward(config["n_embd"])
        self.ln1 = nn.LayerNorm(config["n_embd"])
        self.ln2 = nn.LayerNorm(config["n_embd"])

    def forward(self, x):
        x = x + self.attn(self.ln1(x))
        x = x + self.ff(self.ln2(x))
        return x

class MultiHeadAttention(nn.Module):
    def __init__(self, n_head, head_size):
        super().__init__()
        self.heads = nn.ModuleList([AttentionHead(head_size) for _ in range(n_head)])
        self.proj = nn.Linear(n_head * head_size, head_size * n_head)
        self.dropout = nn.Dropout(dropout)

    def forward(self, x):
        out = torch.cat([h(x) for h in self.heads], dim=-1)
        return self.dropout(self.proj(out))

class AttentionHead(nn.Module):
    def __init__(self, head_size):
        super().__init__()
        self.key = nn.Linear(config["n_embd"], head_size, bias=False)
        self.query = nn.Linear(config["n_embd"], head_size, bias=False)
        self.value = nn.Linear(config["n_embd"], head_size, bias=False)
        self.register_buffer("tril", torch.tril(torch.ones(block_size, block_size)))

    def forward(self, x):
        B, T, C = x.size()
        k = self.key(x)
        q = self.query(x)
        wei = (q @ k.transpose(-2, -1)) * (C ** -0.5)
        wei = wei.masked_fill(self.tril[:T, :T] == 0, float('-inf'))
        wei = F.softmax(wei, dim=-1)
        v = self.value(x)
        return wei @ v

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

model = Transformer(config).to(device)
optimizer = torch.optim.AdamW(model.parameters(), lr=learning_rate)

for iter in range(max_iters):
    if iter % eval_interval == 0:
        losses = evaluate_loss(model)
        print(f"Step {iter}: Train Loss {losses['train']:.4f}, Val Loss {losses['val']:.4f}")

    x, y = get_batch('train')
    _, loss = model(x, y)
    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

# Evaluate on test set
def test_model(model, test_filepath):
    with open(test_filepath, 'r', encoding='utf-8') as f:
        test_text = f.read()
    test_data = torch.tensor([stoi[c] for c in test_text], dtype=torch.long).to(device)
    test_loss = 0
    model.eval()
    with torch.no_grad():
        for i in range(0, len(test_data) - block_size, block_size):
            x = test_data[i:i + block_size].unsqueeze(0)
            y = test_data[i + 1:i + block_size + 1].unsqueeze(0)
            _, loss = model(x, y)
            test_loss += loss.item()
    return test_loss / len(test_data)

test_loss = test_model(model, '/kaggle/input/input-shakespeare-txt/input_shakespeare.txt')
print(f"Test Loss: {test_loss:.4f}")
