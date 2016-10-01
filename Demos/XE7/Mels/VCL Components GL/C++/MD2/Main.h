/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : MD2 models using Mels components demo main form                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Dialogs.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>

// Mels library
#include <UTQRVCLMD2ModelComponentGL.hpp>
#include <UTQRVCLModelComponentGL.hpp>

// std
#include <string>

/**
* MD2 demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TPanel *paModelsTop;
        TQRVCLMD2ModelGL *m2ModelTopLeft;
        TQRVCLMD2ModelGL *m2ModelTopMiddle;
        TQRVCLMD2ModelGL *m2ModelTopRight;
        TPanel *paModelCaptionsTop;
        TLabel *laModelCaptionTopLeft;
        TLabel *laModelCaptionTopMiddle;
        TLabel *laModelCaptionTopRight;
        TPanel *paModelsMiddle;
        TQRVCLMD2ModelGL *m2ModelMiddleLeft;
        TQRVCLMD2ModelGL *m2ModelMiddleMiddle;
        TQRVCLMD2ModelGL *m2ModelMiddleRight;
        TPanel *paModelCaptionsMiddle;
        TLabel *laModeCaptionlMiddleLeft;
        TLabel *laModelCaptionMiddleMiddle;
        TLabel *laModelCaptionMiddleRight;
        TPanel *paModelsBottom;
        TQRVCLMD2ModelGL *m2ModelBottomLeft;
        TQRVCLMD2ModelGL *m2ModelBottomMiddle;
        TQRVCLMD2ModelGL *m2ModelBottomRight;
        TPanel *paModelCaptionsBottom;
        TLabel *laModelCaptionBottomLeft;
        TLabel *laModelCaptionBottomMiddle;
        TLabel *laModelCaptionBottomRight;
        TTimer *tiAnimate;
        TActionList *alActions;
        TAction *acCopyToClipboard;

        void __fastcall tiAnimateTimer(TObject* pSender);
        void __fastcall acCopyToClipboardExecute(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TMainForm(TComponent* pOwner);

        /**
        * Destructor
        */
        virtual __fastcall ~TMainForm();

    private:
        float m_MiddleLeftAngle;
};
extern PACKAGE TMainForm* MainForm;
#endif
