#version 330 core

layout(location = 0) in vec3 vertexPosition;  // Input: vertex position
layout(location = 1) in vec3 vertexColor;     // Input: vertex color

out vec3 fragmentColor; // Output to the fragment shader

void main() {
    gl_Position.xyz = vertexPosition; // Pass position to OpenGL pipeline
    gl_Position.w = 1.0;

    // Pass the color to the fragment shader
    fragmentColor = vertexColor;
}
