/******************************************************************************
 * ==> QR_Renderer -----------------------------------------------------------*
 ******************************************************************************
 * Description : Generic renderer for 3D engine                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Renderer.h"

//------------------------------------------------------------------------------
// QR_Renderer - c++ cross-platform
//------------------------------------------------------------------------------
QR_Renderer::QR_Renderer() : m_pPostProcessingEffect(NULL)
{}
//------------------------------------------------------------------------------
QR_Renderer::~QR_Renderer()
{}
//------------------------------------------------------------------------------
void QR_Renderer::SetPostProcessingEffect(QR_PostProcessingEffect* pEffect)
{
    m_pPostProcessingEffect = pEffect;

    // configure post-processing effect
    if (m_pPostProcessingEffect)
        m_pPostProcessingEffect->Configure();
}
//------------------------------------------------------------------------------
