#version 130
in vec3 VertexPosition;
in vec4 VertexColor;

out vec4 Color;

uniform mat3 RotMat;
uniform mat4 ViewMat; //modelview_matrix
uniform mat4 ProjMat; //projection_matrix

void main() {
  Color = VertexColor;  
  gl_Position = ProjMat * ViewMat * vec4(RotMat * VertexPosition,1.0);
}

