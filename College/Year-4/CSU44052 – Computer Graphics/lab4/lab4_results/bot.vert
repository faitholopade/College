#version 330 core

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec3 vertexNormal;
layout(location = 3) in uvec4 jointIndices;
layout(location = 4) in vec4 jointWeights;

uniform mat4 MVP;
uniform mat4 jointMatrices[64]; // Adjust size if necessary

out vec3 fragNormal;
out vec3 fragPosition;

void main() {
    mat4 skinMatrix = jointWeights.x * jointMatrices[jointIndices.x] +
    jointWeights.y * jointMatrices[jointIndices.y] +
    jointWeights.z * jointMatrices[jointIndices.z] +
    jointWeights.w * jointMatrices[jointIndices.w];

    vec4 skinnedPosition = skinMatrix * vec4(vertexPosition, 1.0);
    vec4 skinnedNormal = skinMatrix * vec4(vertexNormal, 0.0);

    gl_Position = MVP * skinnedPosition;
    fragNormal = normalize(skinnedNormal.xyz);
    fragPosition = skinnedPosition.xyz; // Pass position to fragment shader
}
