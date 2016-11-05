// *************************************************************************************************
// * ==> QR_MathsHelper ---------------------------------------------------------------------------*
// *************************************************************************************************
// * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
// *                                                                                               *
// * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
// * and associated documentation files (the "Software"), to deal in the Software without          *
// * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
// * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
// * Software is furnished to do so, subject to the following conditions:                          *
// *                                                                                               *
// * The above copyright notice and this permission notice shall be included in all copies or      *
// * substantial portions of the Software.                                                         *
// *                                                                                               *
// * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
// * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
// * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
// * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
// * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
// *************************************************************************************************

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
