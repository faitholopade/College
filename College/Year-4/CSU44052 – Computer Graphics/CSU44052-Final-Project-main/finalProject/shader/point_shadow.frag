#version 330 core

in vec3 FragPos;

uniform vec3 lightPos;
uniform float far_plane;


out float FragDepth;

void main()
{
    float lightDistance = length(FragPos - lightPos);

    lightDistance /= far_plane;
    FragDepth = lightDistance;
}
