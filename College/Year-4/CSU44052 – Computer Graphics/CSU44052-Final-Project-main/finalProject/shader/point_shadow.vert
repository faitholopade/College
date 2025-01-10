#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 shadowMatrices[6];
uniform vec3 lightPos;
uniform float far_plane;

out vec3 FragPos;
uniform int activeFaceIndex;


void main()
{

    mat4 shadowMatrix = shadowMatrices[activeFaceIndex];

    gl_Position = shadowMatrix * vec4(aPos, 1.0);

    FragPos = aPos;
}
