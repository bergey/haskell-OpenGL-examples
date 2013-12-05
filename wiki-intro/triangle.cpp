/**
 * From the OpenGL Programming wikibook: http://en.wikibooks.org/wiki/OpenGL_Programming
 * This file is in the public domain.
 * Contributors: Sylvain Beucler
 */
#include <stdio.h>
#include <stdlib.h>
/* Use glew.h instead of gl.h to get all the GL prototypes declared */
#include <GL/glew.h>
/* Using the GLUT library for the base windowing setup */
#include <GL/glut.h>
GLuint program;
GLint attribute_coord2d;
int init_resources()
{
     GLint compile_ok = GL_FALSE, link_ok = GL_FALSE;
     GLuint vs = glCreateShader(GL_VERTEX_SHADER);
     const char *vs_source =
#ifdef GL_ES_VERSION_2_0
          "#version 100\n" // OpenGL ES 2.0
#else
          "#version 120\n" // OpenGL 2.1
#endif
          "attribute vec2 coord2d; "
          "void main(void) { "
          " gl_Position = vec4(coord2d, 0.0, 1.0); "
          "}";
     glShaderSource(vs, 1, &vs_source, NULL);
     glCompileShader(vs);
     glGetShaderiv(vs, GL_COMPILE_STATUS, &compile_ok);
     if (!compile_ok) {
          fprintf(stderr, "Error in vertex shader\n");
          return 0;
     }
     GLuint fs = glCreateShader(GL_FRAGMENT_SHADER);
     const char *fs_source =
#ifdef GL_ES_VERSION_2_0
          "#version 100\n" // OpenGL ES 2.0
#else
          "#version 120\n" // OpenGL 2.1
#endif
          "void main(void) { "
          " gl_FragColor[0] = 0.0; "
          " gl_FragColor[1] = 0.0; "
          " gl_FragColor[2] = 1.0; "
          "}";
     glShaderSource(fs, 1, &fs_source, NULL);
     glCompileShader(fs);
     glGetShaderiv(fs, GL_COMPILE_STATUS, &compile_ok);
     if (!compile_ok) {
          fprintf(stderr, "Error in fragment shader\n");
          return 0;
     }
     program = glCreateProgram();
     glAttachShader(program, vs);
     glAttachShader(program, fs);
     glLinkProgram(program);
     glGetProgramiv(program, GL_LINK_STATUS, &link_ok);
     if (!link_ok) {
          fprintf(stderr, "glLinkProgram:");
          return 0;
     }
     const char* attribute_name = "coord2d";
     attribute_coord2d = glGetAttribLocation(program, attribute_name);
     if (attribute_coord2d == -1) {
          fprintf(stderr, "Could not bind attribute %s\n", attribute_name);
          return 0;
     }
     return 1;
}
void onDisplay()
{
     glClearColor(1.0, 1.0, 1.0, 1.0);
     glClear(GL_COLOR_BUFFER_BIT);
     glUseProgram(program);
     glEnableVertexAttribArray(attribute_coord2d);
     GLfloat triangle_vertices[] = {
          0.0, 0.8,
          -0.8, -0.8,
          0.8, -0.8,
     };
     /* Describe our vertices array to OpenGL (it can't guess its format automatically) */
     glVertexAttribPointer(
          attribute_coord2d, // attribute
          2, // number of elements per vertex, here (x,y)
          GL_FLOAT, // the type of each element
          GL_FALSE, // take our values as-is
          0, // no extra data between each position
          triangle_vertices // pointer to the C array
          );
     /* Push each element in buffer_vertices to the vertex shader */
     glDrawArrays(GL_TRIANGLES, 0, 3);
     glDisableVertexAttribArray(attribute_coord2d);
     glutSwapBuffers();
}
void free_resources()
{
     glDeleteProgram(program);
}
int main(int argc, char* argv[]) {
     glutInit(&argc, argv);
     glutInitDisplayMode(GLUT_RGBA|GLUT_DOUBLE|GLUT_DEPTH);
     glutInitWindowSize(640, 480);
     glutCreateWindow("My First Triangle");
     GLenum glew_status = glewInit();
     if (glew_status != GLEW_OK) {
          fprintf(stderr, "Error: %s\n", glewGetErrorString(glew_status));
          return 1;
     }
     if (init_resources()) {
          glutDisplayFunc(onDisplay);
          glutMainLoop();
     }
     free_resources();
     return 0;
}
