/**************************************************************************************************
 * ==> QR_MathsHelper ----------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Provides some common mathematics tools                                           *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef QR_MathsHelperH
#define QR_MathsHelperH

// std
#include <math.h>

/**
* Maths helper, provides some common mathematics tools
*/
class QR_MathsHelper
{
    public:
        QR_MathsHelper();
        virtual ~QR_MathsHelper();

        /**
        * Checks if a value is a power of 2
        *@param x - value to check
        *@return true if value is a power of two, otherwise false
        */
        static bool IsPowerOfTwo(std::size_t value);

        /**
        * Rounds up to the nearest power of 2
        *@param value - value to round up
        *@return rounded up power of 2
        */
        static inline std::size_t RoundUpToNearestPowerOf2(std::size_t value);

        /**
        * Gets the closest power of 2 from a value
        *@aram value - value
        *@return closest power of 2
        */
        static inline std::size_t GetClosestPowerOf2(std::size_t value);

        /**
        * Converts degrees to radians
        *@param angle - angle in degrees
        *@return angle in radians
        */
        static inline float DegToRad(float angle);

        /**
        * Converts radians to degrees
        *@param angle - angle in radians
        *@return angle in degrees
        */
        static inline float RadToDeg(float angle);
};

//--------------------------------------------------------------------------------------------------
// QR_MathsHelper
//--------------------------------------------------------------------------------------------------
std::size_t QR_MathsHelper::RoundUpToNearestPowerOf2(std::size_t value)
{
    --value;
    value |= value >> 1;
    value |= value >> 2;
    value |= value >> 4;
    value |= value >> 8;
    value |= value >> 16;
    ++value;

    return value;
}
//--------------------------------------------------------------------------------------------------
std::size_t QR_MathsHelper::GetClosestPowerOf2(std::size_t value)
{
    std::size_t pos = 0;

    while (value > 0)
    {
        ++pos;
        value = value >> 1;
    }

    return std::pow(2, pos);
}
//--------------------------------------------------------------------------------------------------
float QR_MathsHelper::DegToRad(float angle)
{
    return ((angle * M_PI) / 180.0f);
}
//--------------------------------------------------------------------------------------------------
float QR_MathsHelper::RadToDeg(float angle)
{
    return ((angle * 180.0f) / M_PI);
}
//--------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------------------
// RAD studio
//--------------------------------------------------------------------------------------------------
#ifdef __CODEGEARC__
    // needed to avoid the W8058 error "Cannot create pre-compiled header: header incomplete" warning in BCC compilers
    ;
#endif
//--------------------------------------------------------------------------------------------------

#endif
