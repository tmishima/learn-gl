#version 130

in vec3 Normal;
in vec3 Position;

in vec4 ShadowCoord;

out vec4 FragColor;

uniform sampler2DShadow ShadowMap;

uniform int shadowSW;

uniform vec3 LightPosition = vec3 (0,10,0);
uniform float MaterialShininess = 0.5;

vec3 phongModelDiffAndSpec()
{
    vec3 n = Normal;
    if( !gl_FrontFacing ) n = -n;
    vec3 s = normalize(vec3(LightPosition) - Position);
    vec3 v = normalize(-Position.xyz);
    vec3 r = reflect( -s, n );
    float sDotN = max( dot(s,n), 0.0 );
    vec3 diffuse = vec3(1.0,1.0,1.0) * sDotN; // Light.Intensity * Material.Kd * sDotN;
    vec3 spec = vec3(0.0);
    if( sDotN > 0.0 )
        spec =  vec3(1.0,1.0,1.0) *    // Light.Intensity * Material.Ks *
            pow( max( dot(r,v), 0.0 ), MaterialShininess );

    return diffuse + spec;
}

void main()
{
  if (shadowSW == 0) 
  {
    //FragColor = vec4(0.0,1.0,0.0,1.0);  // Color;
  }
  else
  {
    vec3 ambient = vec3 (0.2,0.2,0.2);
    vec3 diffAndSpec = phongModelDiffAndSpec();

    float shadow = textureProj(ShadowMap, ShadowCoord);
    FragColor = vec4(diffAndSpec * shadow + ambient, 1.0);
    //FragColor = vec4(1.0,0.0,0.0,1.0);  // Color;
  }
}

