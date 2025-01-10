#version 330 core

in vec3 TexCoords;
out vec4 FragColor;

uniform sampler2D textureSampler;

void main() {
    // Map the texture coordinates to the appropriate face of the cube
    // We only sample the 2D texture (spherical projection could be used)
    vec2 uv = TexCoords.xy * 0.5 + 0.5;  // Convert from [-1, 1] to [0, 1]

    // Sample the texture using the calculated UV coordinates
    FragColor = texture(textureSampler, uv);
}
