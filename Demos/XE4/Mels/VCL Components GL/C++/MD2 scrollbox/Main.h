/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Main form for the scrollbox containing a MD2 model component demo.               *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Eklid library, all right reserved          *
 **************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>

// Mels library
#include "UTQRVCLModelComponentGL.hpp"
#include "UTQRVCLMD2ModelComponentGL.hpp"

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
        TQRVCLMD2ModelGL *m2Model;
        TRichEdit *reQuakeII;
        TButton *btSaveToFile;
        TLabel *laTitle;
        TSaveDialog *sdSave;

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
