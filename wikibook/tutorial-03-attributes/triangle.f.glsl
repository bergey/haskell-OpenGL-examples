uniform float fade;
varying vec3 f_color;

void main(void) {
  gl_FragColor = vec4(f_color.x, f_color.y, f_color.z, fade);
}
