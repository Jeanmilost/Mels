/*****************************************************************************
 * ==> color_vertex.glsl ----------------------------------------------------*
 *****************************************************************************
 * Description : Colored model fragment shader program                       *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#version 150

// input from vertex program
in vec4 qr_fColor;

void main()
{
    // set final color
    gl_FragColor = qr_fColor;
}
