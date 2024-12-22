#version 330 core

in vec3 fragmentColor; // Input from vertex shader

out vec3 color; // Output color to the screen

void main() {
    color = fragmentColor; // Set the color output
}
