/*****************************************************************************
 * ==> default_vertex.glsl --------------------------------------------------*
 *****************************************************************************
 * Description : Vertex shader program, used to draw model                   *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#version 150

// vertex buffer input
in vec3 qr_vPosition;
in vec3 qr_vNormal;
in vec4 qr_vColor;
in vec2 qr_vTexCoord;

// uniform input
uniform mat4  qr_uModel;
uniform mat4  qr_uPerspective;
uniform mat4  qr_uCamera;

// output to fragment shader
out vec4 qr_fColor;
out vec2 qr_fTexCoord;

void main()
{
    // calculate final scene matrix
    mat4 mScene = qr_uPerspective * qr_uCamera * qr_uModel;

    // compute color per vertex
    qr_fColor = qr_vColor;

    // compute texture position per vertex
    qr_fTexCoord = qr_vTexCoord;

    // transform vertex coordinates
    gl_Position = mScene * vec4(qr_vPosition, 1);

    // dummy, it's just to do something with normal, and thus prevent OpenGL to remove it during link
    // optimizations, as normals are not used here (thus done to keep common the draw functions on the
    // app side)
    if (qr_vNormal.x > 1.0)
        gl_Position = mScene * vec4(qr_vPosition, 1);
    else
        gl_Position = mScene * vec4(qr_vPosition, 1);
}
