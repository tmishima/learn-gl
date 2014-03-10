#version 130
in vec3 VertexPosition;
in vec4 VertexColor;
in vec2 VertexTexture;

out vec4 Color;
out vec2 TexCoord;

uniform mat3 RotMat;
uniform mat4 ViewMat; //modelview_matrix
uniform mat4 ProjMat; //projection_matrix

void main() {
  Color = VertexColor;  
  TexCoord = VertexTexture;

  gl_Position = ProjMat * ViewMat * vec4(RotMat * VertexPosition,1.0);
}

