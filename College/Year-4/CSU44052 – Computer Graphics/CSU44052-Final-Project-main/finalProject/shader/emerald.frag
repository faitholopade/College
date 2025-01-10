#version 330 core

in vec2 UV; // from vertex shader
out vec4 fragColor;

uniform sampler2D emeraldTexture;

void main(){
    fragColor = texture(emeraldTexture, UV);
}
