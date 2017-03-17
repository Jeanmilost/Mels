/*************************************************************************************************
 * ==> Main -------------------------------------------------------------------------------------*
 *************************************************************************************************
 * Description : Main form for the scrollbox containing a MD3 model component demo.              *
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

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>

// Mels library
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLMD2ModelComponentGL.hpp>
#include <UTQRVCLMD3ModelComponentGL.hpp>

/**
* Main form
*@note Alpha blending should be enabled in the MD2 model to avoid flickering while the scrollbox is
*      scrolled. In this coontext the alpha blending will behave the same way the DoubleBuffered
*      behave for other controls
*/
class TMainForm : public TForm
{
    __published:
        TScrollBox *sbMain;
        TRichEdit *reQuakeIII;
        TButton *btSaveToFile;
        TLabel *laTitle;
        TSaveDialog *sdSave;
        TQRVCLMD3ModelGL *m3Model;

        void __fastcall FormShow(TObject* pSender);
        void __fastcall btSaveToFileClick(TObject* pSender);

    public:
        __fastcall TMainForm(TComponent* pOwner);

    private:
        /**
        * Update rich edit height
        */
        void UpdateRichEditHeight();
};
extern PACKAGE TMainForm* MainForm;
#endif
