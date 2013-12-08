attribute vec2 coord2d;
attribute vec3 v_color;
varying vec3 f_color;

void main(void) {
  gl_Position = vec4(coord2d, 0.0, 1.0);
  f_color = v_color;
}
