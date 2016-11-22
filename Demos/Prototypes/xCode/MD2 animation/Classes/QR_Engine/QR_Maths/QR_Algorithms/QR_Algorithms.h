/******************************************************************************
 * ==> QR_Algorithms ---------------------------------------------------------*
 ******************************************************************************
 * Description : Some c++ common algorithms                                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_AlgorithmsH
#define QR_AlgorithmsH

// std
#include <cmath>

// qr engine
#include "QR_Types.h"

/**
* Some c++ common algorithms
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Algorithms
{
    public:
        QR_Algorithms();
        virtual ~QR_Algorithms();

        /**
        * Gets PI value
        *@return PI value
        */
        static inline M_Precision GetPI();

        /**
        * Convert angle in degrees to angle in radians
        *@param angle - angle in degrees to convert
        *@return converted angle in radians
        */
        static inline M_Precision DegToRad(const M_Precision& angle);

        /**
        * Convert angle in radians to angle in degrees
        *@param angle - angle in radians to convert
        *@return converted angle in degrees
        */
        static inline M_Precision RadToDeg(const M_Precision& angle);
};

//------------------------------------------------------------------------------
// Implementation
//------------------------------------------------------------------------------
M_Precision QR_Algorithms::GetPI()
{
    return std::atan(1.0f) * 4.0f;
}
//------------------------------------------------------------------------------
M_Precision QR_Algorithms::DegToRad(const M_Precision& angle)
{
    // convert degrees to radians and return value
    return ((angle * (2.0f * GetPI())) / 360.0f);
}
//------------------------------------------------------------------------------
M_Precision QR_Algorithms::RadToDeg(const M_Precision& angle)
{
    // convert radians to degrees and return value
    return ((angle * 360.0f) / (2.0f * GetPI()));
}
//------------------------------------------------------------------------------

#endif // QR_AlgorithmsH
