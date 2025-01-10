#version 330 core

layout(location = 0) in vec3 vertexPosition;

uniform mat4 MVP;
out vec3 worldPos;

void main() {
    gl_Position = MVP * vec4(vertexPosition, 1.0);
    worldPos = vertexPosition;
}
