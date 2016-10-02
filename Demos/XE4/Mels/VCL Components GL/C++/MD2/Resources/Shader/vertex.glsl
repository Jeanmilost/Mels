/*****************************************************************************
 * ==> vertex.glsl ----------------------------------------------------------*
 *****************************************************************************
 * Description : Textured (and interpolated) model vertex shader program     *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#version 150

// vertex buffer input
in vec3 qr_vPosition;
in vec3 qr_viPosition;
in vec4 qr_vColor;
in vec2 qr_vTexCoord;

// uniform input
uniform mat4  qr_uModel;
uniform mat4  qr_uPerspective;
uniform mat4  qr_uCamera;
uniform float qr_fInterpolation;

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

    vec3 vPosition;

    // process vertex interpolation
    if (qr_fInterpolation <= 0.0)
        vPosition = qr_vPosition;
    else
    if (qr_fInterpolation >= 1.0)
        vPosition = qr_viPosition;
    else
        vPosition = qr_vPosition + qr_fInterpolation * (qr_viPosition - qr_vPosition);

    // transform vertex coordinates
    gl_Position = mScene * vec4(vPosition, 1);
}
