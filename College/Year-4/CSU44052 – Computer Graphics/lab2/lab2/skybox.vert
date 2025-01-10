#version 330 core

layout(location = 0) in vec3 vertexPosition;

uniform mat4 MVP;

out vec3 TexCoords;

void main() {
    // Pass the vertex position as the texture coordinates
    TexCoords = vertexPosition;

    // Transform the vertex position with the MVP matrix
    gl_Position = MVP * vec4(vertexPosition, 1.0);
}
