#version 130

in vec3 Normal;
in vec3 Position;

in vec4 ShadowCoord;

out vec4 FragColor;

uniform sampler2DShadow ShadowMap;
//uniform sampler2D ShadowMap;

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
  vec3 ambient = vec3 (0.2,0.2,0.2);
  //vec3 diffAndSpec = phongModelDiffAndSpec();
  vec3 diffAndSpec = vec3 (0.8,0.8,0.8);

  float shadow = textureProj(ShadowMap, ShadowCoord);
  if( shadow < ShadowCoord.x )
  {
    //FragColor = vec4(diffAndSpec * shadow + ambient, 1.0);
    FragColor = vec4(diffAndSpec + ambient, 1.0);
  }
  else{
    FragColor = vec4(0.0,0.0,0.0, 1.0);
  }
  
  //if( textureProj(ShadowMap, ShadowCoord) < ShadowCoord.z)
  //{
  //  FragColor = vec4(1.0,1.0,1.0,1.0);
  //}
  //else{
  //  FragColor = vec4(0.0,0.0,0.0,1.0);
  //}
  //FragColor = shadow2DProj (ShadowMap, ShadowCoord);
}


