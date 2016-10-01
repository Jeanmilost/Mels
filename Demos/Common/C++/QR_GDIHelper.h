/**************************************************************************************************
 * ==> QR_GDIHelper ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Somme tools to manipulate the Windows Graphics Device Interface (GDI)            *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef QR_GDIHelperH
#define QR_GDIHelperH

// vcl
#include <Vcl.Graphics.hpp>

/**
* Somme tools to manipulate the Windows Graphics Device Interface (GDI)
*@author Jean-Milost Reymond
*/
class QR_GDIHelper
{
    public:
        QR_GDIHelper();
        virtual ~QR_GDIHelper();

        /**
        * Applies antialiasing from a source bitmap to a destination bitmap
        *@param pSource - source bitmap
        *@param pDest - destination bitmap
        *@param factor - antialiasing factor, should correspond to the difference of size between
        *                source and destination bitmaps (e.g. for a source of 400x400 pixels and a
        *                destination of 100x100 pixels, the factor should be 4)
        */
        static void ApplyAntialiasing(TBitmap* pSource, TBitmap* pDest, std::size_t factor);
};

#endif
