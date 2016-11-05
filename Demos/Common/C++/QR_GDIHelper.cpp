// *************************************************************************************************
// * ==> QR_GDIHelper -----------------------------------------------------------------------------*
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
