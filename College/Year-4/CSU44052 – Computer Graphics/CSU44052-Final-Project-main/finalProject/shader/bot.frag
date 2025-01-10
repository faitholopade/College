#version 330 core

in vec3 worldPosition;
in vec3 worldNormal;

out vec3 finalColor;

uniform vec3 lightPosition;
uniform vec3 lightIntensity;

// for point shadows:
uniform samplerCube depthCubemap;
uniform float far_plane;

// compute point shadow
float calcPointShadow(vec3 fragPos, vec3 lightPos)
{
    vec3 fragToLight = fragPos - lightPos;
    float currentDist = length(fragToLight);

    float storedDepth = texture(depthCubemap, fragToLight).r * far_plane;

    float bias = 0.01;
    // 1.0 = in shadow 0.0 = not in shadow
    float shadow = currentDist - bias > storedDepth ? 1.0 : 0.0;
    return shadow;
}

void main()
{
    vec3 lightDir = lightPosition - worldPosition;
    float lightDist = dot(lightDir, lightDir);
    lightDir = normalize(lightDir);

    vec3 v = lightIntensity * clamp(dot(lightDir, worldNormal),0.0,1.0)/lightDist;
    v = v/(1.0+v);

    // point shadow:
    float shadow = calcPointShadow(worldPosition, lightPosition);
    v *= (1.0 - shadow);

    finalColor = pow(v,vec3(1.0/2.2));
}
