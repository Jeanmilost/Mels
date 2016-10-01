/**************************************************************************************************
 * ==> QR_GDIHelper ------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Somme tools to manipulate the Windows Graphics Device Interface (GDI)            *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include "QR_GDIHelper.h"

// std
#include <memory>

//--------------------------------------------------------------------------------------------------
// QR_GDIHelper - c++ Windows and RAD Studio VCL dependent
//--------------------------------------------------------------------------------------------------
QR_GDIHelper::QR_GDIHelper()
{}
//--------------------------------------------------------------------------------------------------
QR_GDIHelper::~QR_GDIHelper()
{}
//--------------------------------------------------------------------------------------------------
void QR_GDIHelper::ApplyAntialiasing(TBitmap* pSource, TBitmap* pDest, std::size_t factor)
{
    // no source bitmap?
    if (!pSource)
        return;

    // no destination bitmap?
    if (!pDest)
        return;

    // configure destination bitmap
    pDest->PixelFormat = pSource->PixelFormat;
    pDest->AlphaFormat = pSource->AlphaFormat;
    pDest->SetSize(pSource->Width / factor, pSource->Height / factor);

    // set stretch mode to half tones (thus resizing will be smooth)
    int prevMode = ::SetStretchBltMode(pDest->Canvas->Handle, HALFTONE);

    try
    {
        // apply antialiasing on the destination image
        ::StretchBlt(pDest->Canvas->Handle,
                     0,
                     0,
                     pDest->Width,
                     pDest->Height,
                     pSource->Canvas->Handle,
                     0,
                     0,
                     pSource->Width,
                     pSource->Height,
                     SRCCOPY);
    }
    __finally
    {
        // restore previous stretch blit mode
        ::SetStretchBltMode(pDest->Canvas->Handle, prevMode);
    }
}
//--------------------------------------------------------------------------------------------------
