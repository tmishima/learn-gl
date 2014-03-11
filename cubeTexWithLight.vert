#version 130
in vec3 VertexPosition;
in vec4 VertexColor;
in vec2 VertexTexture;
in vec3 VertexNormal;

out vec4 Color;
out vec2 TexCoord;

uniform mat3 RotMat;
uniform mat4 ProjViewMat; //projection_matrix modelview_matrix
//uniform mat4 invProjViewMat;

uniform vec4 ambientColor = vec4 (0.2,0.2,0.2,0.2);
uniform vec3 lightDirection = vec3 (5,5,5);

void main() {
  //vec3 invLight = normalize ( invProjViewMat * vec4 (lightDirection,0.0)).xyz;
  //float diffuse = clamp (dot (RotMat * VertexNormal, invLight), 0.2, 1.0);
  //vec3 invLight = normalize ( invProjViewMat * vec4 (lightDirection,0.0)).xyz;
  vec3 invLight = normalize (lightDirection);
  float diffuse = clamp (dot (RotMat * VertexNormal, invLight), 0.0, 1.0);

  Color = VertexColor * vec4( vec3(diffuse), 1.0) + ambientColor;  
  TexCoord = VertexTexture;

  gl_Position = ProjViewMat * vec4(RotMat * VertexPosition,1.0);
}

