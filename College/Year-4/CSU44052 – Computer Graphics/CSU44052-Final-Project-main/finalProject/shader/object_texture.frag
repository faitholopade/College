#version 330 core

in vec2 UV;
in vec3 FragPos;
in mat3 TBN;

out vec4 fragColor;

uniform sampler2D diffuseTex;
uniform sampler2D normalMap;
uniform vec3 baseColor;

uniform vec3 lightPos;
uniform vec3 viewPos;

uniform samplerCube depthCubemap;
uniform float far_plane;

float calcPointShadow(vec3 fragPos, vec3 lightPos)
{
    vec3 fragToLight = fragPos - lightPos;
    float currentDist = length(fragToLight);

    float storedDepth = texture(depthCubemap, fragToLight).r * far_plane;

    float bias = 0.01;
    float shadow = currentDist - bias > storedDepth ? 1.0 : 0.0;
    return shadow;
}

void main(){
    vec3 diffColor = texture(diffuseTex, UV).rgb * baseColor;

    // normal from normal map
    vec3 normal = texture(normalMap, UV).rgb;
    normal = normalize(normal * 2.0 - 1.0); // from [0,1] to [-1,1]
    normal = normalize(TBN * normal);

    vec3 lightDir = normalize(lightPos - FragPos);
    float diff = max(dot(normal, lightDir),0.0);

    vec3 ambient = 0.1 * diffColor;
    vec3 diffuse = diff * diffColor;

    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir,normal);
    float spec = pow(max(dot(viewDir,reflectDir),0.0),16.0);
    vec3 specular = spec * vec3(1.0);

    vec3 color = ambient + diffuse + specular;

    float shadow = calcPointShadow(FragPos, lightPos);
    color *= (1.0 - shadow);

    fragColor = vec4(color,1.0);
}
