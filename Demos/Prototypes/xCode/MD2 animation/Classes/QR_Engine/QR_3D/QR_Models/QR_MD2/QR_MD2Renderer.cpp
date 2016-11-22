/******************************************************************************
 * ==> QR_MD2Renderer --------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 model renderer                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD2Renderer.h"

//------------------------------------------------------------------------------
// Class QR_MD2Renderer - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Renderer::QR_MD2Renderer() :
    QR_ModelRenderer(),
    m_pModel(NULL),
    m_pAnimation(NULL)
{
    m_pModel     = new QR_MD2();
    m_pAnimation = new QR_MD2Animation(m_pModel);
}
//------------------------------------------------------------------------------
QR_MD2Renderer::~QR_MD2Renderer()
{
    // delete MD2 model animation
    if (m_pAnimation)
        delete m_pAnimation;

    // delete MD2 model
    if (m_pModel)
        delete m_pModel;
}
//------------------------------------------------------------------------------
QR_MD2* QR_MD2Renderer::GetModel() const
{
    return m_pModel;
}
//------------------------------------------------------------------------------
QR_MD2Animation* QR_MD2Renderer::GetAnimation() const
{
    return m_pAnimation;
}
//------------------------------------------------------------------------------
void QR_MD2Renderer::SetUseCache(bool value)
{
    if (!m_pAnimation)
        return;

    m_pAnimation->SetUseCache(value);
}
//------------------------------------------------------------------------------
