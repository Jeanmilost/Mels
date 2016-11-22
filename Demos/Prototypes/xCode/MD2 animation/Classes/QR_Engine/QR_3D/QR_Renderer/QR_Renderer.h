/******************************************************************************
 * ==> QR_Renderer -----------------------------------------------------------*
 ******************************************************************************
 * Description : Generic renderer for 3D engine                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_RendererH
#define QR_RendererH

// std
#include <string>
#include <vector>

// qr engine
#include "QR_Types.h"
#include "QR_Color.h"
#include "QR_PostProcessingEffect.h"

/**
* Generic renderer for 3D engine
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Renderer
{
    public:
        /**
        * Scene flags
        */
        enum IESceneFlags
        {
            IE_SF_None       = 0x00,
            IE_SF_ClearColor = 0x01,
            IE_SF_ClearDepth = 0x02,
        };

        /**
        * Constructor
        */
        QR_Renderer();

        /**
        * Destructor
        */
        virtual ~QR_Renderer();

        /**
        * Called when renderer was initialized and should be configured
        *@param pRenderer - event sender
        */
        typedef void (*ITfOnConfigure)(const QR_Renderer* pRenderer);

        /**
        * Set post-processing effect
        *@param pEffect - post-processing effect (e.g. anti aliasing) to apply
        */
        virtual void SetPostProcessingEffect(QR_PostProcessingEffect* pEffect);

        /**
        * Begins a scene
        *@param color - scene background color
        *@param flags - scene flags
        */
        virtual void BeginScene(const QR_Color& color, IESceneFlags flags) const = 0;

        /**
        * Ends a scene
        */
        virtual void EndScene() const = 0;

    protected:
        QR_PostProcessingEffect* m_pPostProcessingEffect;
};

#endif // QR_RendererH
