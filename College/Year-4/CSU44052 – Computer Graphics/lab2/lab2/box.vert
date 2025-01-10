#version 330 core

// Input
layout(location = 0) in vec3 vertexPosition;
layout(location = 2) in vec2 vertexUV;

// Output data to pass to the fragment shader
out vec2 UV;

// Matrix for vertex transformation
uniform mat4 MVP;

void main() {
    // Transform the vertex position and pass it to the fragment shader
    gl_Position = MVP * vec4(vertexPosition, 1.0);

    // Pass the UV coordinates to the fragment shader
    UV = vertexUV;
}
