#version 130
in vec3 VertexPosition;
in vec3 VertexNormal;

out vec3 Normal;
out vec3 Position;

out vec4 ShadowCoord;

//uniform mat4 ModelViewMatrix; //projection_matrix modelview_matrix
uniform mat3 NormalMatrix;
uniform mat4 shadowScale = mat4( 0.5, 0.0, 0.0, 0.0,
                                 0.0, 0.5, 0.0, 0.0,
                                 0.0, 0.0, 0.5, 0.0,
                                 0.5, 0.5, 0.5, 1.0
                               );
uniform mat4 ShadowMatrix;
uniform mat4 ViewMat; //modelview_matrix
uniform mat4 ProjMat; //projection_matrix


uniform mat3 RotMat = mat3 ( 1.0, 0.0, 0.0,
                             0.0, 1.0, 0.0,
                             0.0, 0.0, 1.0
                           );
uniform mat4 ViewMat; //modelview_matrix
uniform mat4 ProjMat; //projection_matrix

void main() {
  Position = (ModelViewMatrix * vec4(VertexPosition,1.0)).xyz;
  //Position = (ProjMat * ViewMat * vec4(RotMat * VertexPosition,1.0)).xyz;
  Normal = normalize ( NormalMatrix * VertexNormal );

  ShadowCoord = shadowScale * ShadowMatrix * vec4 (VertexPosition, 1.0);
  gl_Position = ProjMat * ViewMat * vec4(RotMat * VertexPosition,1.0);
}

