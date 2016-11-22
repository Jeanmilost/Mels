/******************************************************************************
 * ==> QR_ModelRenderer ------------------------------------------------------*
 ******************************************************************************
 * Description : Model renderer                                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MODELRENDERERH
#define QR_MODELRENDERERH

// qr engine
#include "QR_Types.h"
#include "QR_Shader.h"

/**
* Model renderer
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_ModelRenderer
{
    public:
        /**
        * Called when draw begins
        *@param pRenderer - model renderer
        *@param pShader - shader used to draw model
        */
        typedef void (*ITfOnDrawBegin)(      QR_ModelRenderer* pRenderer,
                                       const QR_Shader*        pShader);

        /**
        * Called when draw ends
        *@param pRenderer - model renderer
        */
        typedef void (*ITfOnDrawEnd)(QR_ModelRenderer* pRenderer);

        /**
        * Constructor
        */
        QR_ModelRenderer();

        /**
        * Destructor
        */
        virtual ~QR_ModelRenderer();

        /**
        * Sets model shader to use
        *@param pShader - shader to use
        */
        #ifndef USE_OPENGL_DIRECT_MODE
            virtual void SetShader(QR_Shader* pShader);
        #endif // USE_OPENGL_DIRECT_MODE

        /**
        * Draws the model
        *@param elapsedTime - elapsed time since last frame was drawn
        *@param fps - number of frames per seconds
        */
        virtual void Draw(const QR_Double& elapsedTime, QR_SizeT fps) = 0;

        /**
        * Sets OnDrawBegin callback
        *@param fHandler - function handler
        */
        void Set_OnDrawBegin(ITfOnDrawBegin fHandler);

        /**
        * Sets OnDrawEnd callback
        *@param fHandler - function handler
        */
        void Set_OnDrawBegin(ITfOnDrawEnd fHandler);

    protected:
        #ifndef USE_OPENGL_DIRECT_MODE
            QR_Shader* m_pShader;
        #endif // USE_OPENGL_DIRECT_MODE

        ITfOnDrawBegin m_fOnDrawBegin;
        ITfOnDrawEnd   m_fOnDrawEnd;
};

#endif // QR_MODELRENDERERH
