#version 330 core
// layout(location = 0) in vec3 vertexPosition_modelspace;

void main() {
  gl_Position = ftransform(); // vertexPosition_modelspace;
//   // gl_Position.w = 1.0;
}

// uniform Transformation {
//         mat4 projection_matrix;
//         mat4 modelview_matrix;
// };

// in vec3 vertex;

// void main(void) {
//         gl_Position = projection_matrix * modelview_matrix * vec4(vertex, 1.0);
// }