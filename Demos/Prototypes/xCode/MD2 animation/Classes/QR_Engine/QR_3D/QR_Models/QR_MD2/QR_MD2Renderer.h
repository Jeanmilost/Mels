/******************************************************************************
 * ==> QR_MD2Renderer --------------------------------------------------------*
 ******************************************************************************
 * Description : MD2 model renderer                                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD2RENDERERH
#define QR_MD2RENDERERH

// qr engine
#include "QR_ModelRenderer.h"
#include "QR_MD2.h"
#include "QR_MD2Animation.h"

/**
* MD2 model renderer
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD2Renderer : public QR_ModelRenderer
{
    public:
        /**
        * Constructor
        */
        QR_MD2Renderer();

        /**
        * Destructor
        */
        virtual ~QR_MD2Renderer();

        /**
        * Gets MD2 model
        *@return MD2 model
        */
        virtual QR_MD2* GetModel() const;

        /**
        * Gets MD2 animation
        *@return MD2 animation
        */
        virtual QR_MD2Animation* GetAnimation() const;

        /**
        * Enables or disables cache
        *@param value - if true, cache is enabled, otherwise disabled
        */
        virtual void SetUseCache(bool value);

        /**
        * Initializes model
        */
        virtual void Initialize() = 0;

    protected:
        QR_MD2*          m_pModel;
        QR_MD2Animation* m_pAnimation;
};

#endif // QR_MD2RENDERERH
