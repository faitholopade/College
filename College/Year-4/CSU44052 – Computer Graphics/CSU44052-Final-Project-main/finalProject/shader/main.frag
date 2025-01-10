#version 330 core

in vec3 worldPos;
out vec4 fragColor;

uniform vec3 baseColor;

void main() {
	fragColor = vec4(baseColor, 1.0);
}
