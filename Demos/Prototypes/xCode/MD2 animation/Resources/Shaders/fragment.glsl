/*****************************************************************************
 * ==> fragment.glsl --------------------------------------------------------*
 *****************************************************************************
 * Description : Fragment shader program                                     *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

// input from vertex shader
varying lowp vec4 qr_fColor;

void main()
{
    // set final color
    gl_FragColor = qr_fColor;
}
