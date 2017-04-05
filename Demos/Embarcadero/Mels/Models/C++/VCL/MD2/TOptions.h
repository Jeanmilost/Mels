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

#ifndef TOptionsH
#define TOptionsH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>

/**
* MD2 demo options
*/
class TOptions : public TForm
{
    __published:
        TCheckBox *ckFullScreen;
        TCheckBox *ckUseShader;
        TCheckBox *ckUsePreCalculatedLighting;
        TCheckBox *ckCollisions;
        TLabel *laFPS;
        TEdit *edFPS;
        TPanel *paFPS;
        TUpDown *udFPS;
        TButton *btOk;

        void __fastcall FormCreate(TObject* pSender);
        void __fastcall edFPSExit(TObject* pSender);
        void __fastcall btOkClick(TObject* pSender);

    public:
        __fastcall TOptions(TComponent* pOwner);
};
extern PACKAGE TOptions* Options;
#endif
