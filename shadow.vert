#version 130
in vec3 VertexPosition;

uniform vec4 mvpMatrix;

void main() {
  gl_Position = mvpMatrix * vec4(VertexPosition,1.0);
}

