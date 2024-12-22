#version 330 core

// Input from the vertex shader
in vec2 UV;

// Output color
out vec4 finalColor;

// Texture sampler
uniform sampler2D textureSampler;

void main() {
    // Sample the color from the texture using the UV coordinates
    vec3 textureColor = texture(textureSampler, UV).rgb;

    // Set the final color to the texture color
    finalColor = vec4(textureColor, 1.0);
}
