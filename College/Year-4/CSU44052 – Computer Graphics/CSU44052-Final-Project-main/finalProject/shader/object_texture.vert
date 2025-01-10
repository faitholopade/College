#version 330 core

layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec2 vertexUV;
layout(location = 2) in vec3 vertexNormal;
layout(location = 3) in vec3 vertexTangent;
layout(location = 4) in vec3 vertexBitangent;

uniform mat4 MVP;
uniform mat4 Model;
uniform mat4 View;
uniform mat3 NormalMatrix; // normal transformations

out vec2 UV;
out vec3 FragPos;
out mat3 TBN;

void main(){
    gl_Position = MVP * vec4(vertexPosition,1.0);
    UV = vertexUV;

    vec4 worldPos = Model * vec4(vertexPosition,1.0);
    FragPos = worldPos.xyz;

    vec3 T = normalize(mat3(Model)*vertexTangent);
    vec3 B = normalize(mat3(Model)*vertexBitangent);
    vec3 N = normalize(mat3(Model)*vertexNormal);
    TBN = mat3(T,B,N);
}
