#version 330 core

// Input attributes
layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec3 vertexColor;
layout(location = 2) in vec3 vertexNormal;

// Outputs to fragment shader
out vec3 color;
out vec3 worldPosition;
out vec3 worldNormal;

// Uniforms
uniform mat4 MVP;         // View-projection * Model
uniform mat4 modelMatrix; // Model matrix
uniform mat4 normalMatrix;// inverse-transpose of model

void main()
{
    // Final clip-space
    gl_Position = MVP * vec4(vertexPosition, 1.0);

    color = vertexColor;

    // Transform position to world space
    worldPosition = vec3(modelMatrix * vec4(vertexPosition, 1.0));

    // Transform normal with inverse-transpose
    worldNormal = normalize(vec3(normalMatrix * vec4(vertexNormal, 0.0)));
}
