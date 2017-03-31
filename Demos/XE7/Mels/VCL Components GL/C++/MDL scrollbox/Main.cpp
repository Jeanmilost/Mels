/*************************************************************************************************
 * ==> Main -------------------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : Main form for the scrollbox containing a MD2 model component demo.              *
 * Developer   : Jean-Milost Reymond                                                             *
 *************************************************************************************************
 * MIT License - The Mels Library, a free and easy-to-use 3D Models library                      *
 *                                                                                               *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software *
 * and associated documentation files (the "Software"), to deal in the Software without          *
 * restriction, including without limitation the rights to use, copy, modify, merge, publish,    *
 * distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the *
 * Software is furnished to do so, subject to the following conditions:                          *
 *                                                                                               *
 * The above copyright notice and this permission notice shall be included in all copies or      *
 * substantial portions of the Software.                                                         *
 *                                                                                               *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING *
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND    *
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,  *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING      *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *
 *************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <memory>

#pragma package(smart_init)
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLMD2ModelComponentGL"
#pragma link "UTQRVCLMDLModelComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner)
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject* pSender)
{
    if (mlModel->CanFocus())
        mlModel->SetFocus();

    UpdateRichEditHeight();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btSaveToFileClick(TObject* pSender)
{
    if (!sdSave->Execute())
        return;

    // create a bitmap to receive the model
    std::auto_ptr<TBitmap> pBitmap(new TBitmap());
    pBitmap->PixelFormat = pf32bit;
    pBitmap->AlphaFormat = afPremultiplied;
    pBitmap->SetSize(100, 100);

    // draw model into bitmap
    ::SendMessage(mlModel->Handle, WM_PRINTCLIENT, LPARAM(pBitmap->Canvas->Handle), 0);

    // save bitmap
    pBitmap->SaveToFile(sdSave->FileName);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::UpdateRichEditHeight()
{
    HDC hDC = ::GetDC(reQuake->Handle);

    TTextMetric metrics;

    try
    {
        HFONT hSaveFont = (HFONT)::SelectObject(hDC, reQuake->Handle);
        ::GetTextMetrics(hDC, &metrics);
        ::SelectObject(hDC, hSaveFont);
    }
    __finally
    {
        ::ReleaseDC(reQuake->Handle, hDC);
    }

    int lineHeight = metrics.tmHeight;
    int increase   = reQuake->Height;
    int lc         = reQuake->Lines->Count;

    if (lc < 1)
        lc = 1;

    reQuake->Height         = (lc * lineHeight)       + 8;
    increase                = reQuake->Height         - increase;
    reQuake->Parent->Height = reQuake->Parent->Height + increase;
}
//--------------------------------------------------------------------------------------------------
