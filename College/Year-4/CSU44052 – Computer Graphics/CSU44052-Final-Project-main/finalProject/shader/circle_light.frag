#version 330 core
uniform vec3 circleColor;
out vec4 FragColor;

void main()
{
    FragColor = vec4(circleColor, 1.0);
}
