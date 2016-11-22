/******************************************************************************
 * ==> QR_ModelRenderer ------------------------------------------------------*
 ******************************************************************************
 * Description : Model renderer                                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_ModelRenderer.h"

//------------------------------------------------------------------------------
// Class QR_ModelRenderer - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelRenderer::QR_ModelRenderer() :
    #ifndef USE_OPENGL_DIRECT_MODE
        m_pShader(NULL),
    #endif // USE_OPENGL_DIRECT_MODE
    m_fOnDrawBegin(NULL),
    m_fOnDrawEnd(NULL)
{}
//------------------------------------------------------------------------------
QR_ModelRenderer::~QR_ModelRenderer()
{}
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_ModelRenderer::SetShader(QR_Shader* pShader)
    {
        m_pShader = pShader;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
void QR_ModelRenderer::Set_OnDrawBegin(ITfOnDrawBegin fHandler)
{
    m_fOnDrawBegin = fHandler;
}
//------------------------------------------------------------------------------
void QR_ModelRenderer::Set_OnDrawBegin(ITfOnDrawEnd fHandler)
{
    m_fOnDrawEnd = fHandler;
}
//------------------------------------------------------------------------------
