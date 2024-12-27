#version 330 core

in vec3 color;
in vec3 worldPosition;
in vec3 worldNormal;

out vec4 finalColor;

// Uniforms
uniform vec3 lightPosition;
uniform vec3 lightIntensity;

uniform sampler2D shadowMap;      // Depth texture from the light’s POV
uniform mat4 lightMVP;            // Light-space MVP
uniform float shadowBias;
uniform int enableShadows;

uniform float exposure;  // For tone mapping scale

void main()
{
	// ===== 1) Lambertian lighting from a point light =====
	vec3 N = normalize(worldNormal);
	vec3 L = lightPosition - worldPosition;
	float dist2 = dot(L, L);
	float r = sqrt(dist2);
	L = L / r; // normalized direction
	float NdotL = max(dot(N, L), 0.0);

	// Lambert = (Kd * LightIntensity * (NdotL / dist^2))
	vec3 lambert = color * lightIntensity * (NdotL / dist2);

	// ===== 2) Shadow Mapping =====
	float shadowFactor = 1.0;
	if (enableShadows == 1)
	{
		// Project worldPosition into light’s clip space
		vec4 lightSpacePos = lightMVP * vec4(worldPosition, 1.0);
		// Perspective divide
		lightSpacePos.xyz /= lightSpacePos.w;
		// Transform from [-1..1] to [0..1]
		vec3 uv = lightSpacePos.xyz * 0.5 + 0.5;

		// If within the light frustum, do the shadow test
		if (uv.x >= 0.0 && uv.x <= 1.0 &&
		uv.y >= 0.0 && uv.y <= 1.0 &&
		uv.z >= 0.0 && uv.z <= 1.0)
		{
			float closestDepth = texture(shadowMap, uv.xy).r;
			float currentDepth = uv.z;
			// Compare + small bias
			if (currentDepth - shadowBias > closestDepth)
			shadowFactor = 0.2; // in shadow
		}
	}

	vec3 litColor = lambert * shadowFactor;

	// ===== 3) Tone mapping (Reinhard-ish) =====
	// c' = c / (1 + c*exposure)
	// Then gamma correct with gamma=2.2
	litColor = litColor / (vec3(1.0) + litColor * exposure);
	litColor = pow(litColor, vec3(1.0 / 2.2));

	finalColor = vec4(litColor, 1.0);
}
