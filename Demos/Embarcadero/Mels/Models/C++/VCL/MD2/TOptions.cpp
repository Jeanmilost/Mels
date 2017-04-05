// *************************************************************************************************
// * ==> TOptions ---------------------------------------------------------------------------------*
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

#include <vcl.h>
#pragma hdrstop

#include "TOptions.h"

#pragma package(smart_init)
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// Global defines
//--------------------------------------------------------------------------------------------------
TOptions* Options;
//--------------------------------------------------------------------------------------------------
// TOptions
//--------------------------------------------------------------------------------------------------
__fastcall TOptions::TOptions(TComponent* pOwner) :
    TForm(pOwner)
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::FormCreate(TObject* pSender)
{
    // to see form in taskbar even if main form is still not created
    ::SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::edFPSExit(TObject *Sender)
{
    // check if FPS is out of bounds
    if (::StrToInt(edFPS->Text) > udFPS->Max)
        edFPS->Text = ::IntToStr(udFPS->Max);
    else
    if (::StrToInt(edFPS->Text) < udFPS->Min)
        edFPS->Text = ::IntToStr(udFPS->Min);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TOptions::btOkClick(TObject* pSender)
{
    Close();
}
//--------------------------------------------------------------------------------------------------
