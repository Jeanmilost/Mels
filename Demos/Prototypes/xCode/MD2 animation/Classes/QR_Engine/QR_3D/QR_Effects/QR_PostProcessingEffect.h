/******************************************************************************
 * ==> QR_PostProcessingEffect -----------------------------------------------*
 ******************************************************************************
 * Description : Post-processing effect interface                             *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_PostProcessingEffectH
#define QR_PostProcessingEffectH

// qr engine
#include "QR_Effect.h"

/**
* Post-processing effect interface
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_PostProcessingEffect : public QR_Effect
{
    public:
        /**
        * Constructor
        */
        QR_PostProcessingEffect();

        /**
        * Destructor
        */
        virtual ~QR_PostProcessingEffect();

        /**
        * Begins effect
        */
        virtual void Begin() = 0;

        /**
        * Ends effect
        */
        virtual void End() = 0;
};

#endif // QR_PostProcessingEffectH
