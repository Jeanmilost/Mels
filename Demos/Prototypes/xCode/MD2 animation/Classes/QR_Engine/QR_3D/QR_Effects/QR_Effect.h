/******************************************************************************
 * ==> QR_Effect -------------------------------------------------------------*
 ******************************************************************************
 * Description : Effect interface                                             *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_EffectH
#define QR_EffectH

/**
* Effect interface
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Effect
{
    public:
        /**
        * Constructor
        */
        QR_Effect();

        /**
        * Destructor
        */
        virtual ~QR_Effect();

        /**
        * Configures effect
        */
        virtual void Configure() = 0;
};

#endif // QR_EFFECT_H
