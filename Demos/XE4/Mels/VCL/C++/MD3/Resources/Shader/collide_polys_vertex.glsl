/*****************************************************************************
 * ==> collide_polys_vertex.glsl --------------------------------------------*
 *****************************************************************************
 * Description : Vertex shader program, used to show polygons in collision   *
 *               with mouse pointer                                          *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#version 150

// vertex buffer input
in vec3 qr_vPosition;
in vec4 qr_vColor;

// uniform input
uniform mat4  qr_uModel;
uniform mat4  qr_uPerspective;
uniform mat4  qr_uCamera;

// output to fragment shader
out vec4 qr_fColor;

void main()
{
    // calculate final scene matrix
    mat4 mScene = qr_uPerspective * qr_uCamera * qr_uModel;

    // compute color per vertex
    qr_fColor = qr_vColor;

    // transform vertex coordinates
    gl_Position = mScene * vec4(qr_vPosition, 1);
}
