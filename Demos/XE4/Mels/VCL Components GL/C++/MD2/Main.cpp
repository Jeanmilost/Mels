/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : MD2 models using Mels components demo main form                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

#include <memory>

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRVCLMD2ModelComponentGL"
#pragma link "UTQRVCLModelComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_MiddleLeftAngle(-0.7854)
{}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tiAnimateTimer(TObject *Sender)
{
    // calculate next angle
    m_MiddleLeftAngle += 0.05f;

    // is angle exceeding the max limit?
    if (m_MiddleLeftAngle >= M_PI * 2.0f)
        m_MiddleLeftAngle -= M_PI * 2.0f;

    // apply rotation to model
    m2ModelMiddleLeft->Model->RotationY = m_MiddleLeftAngle;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::acCopyToClipboardExecute(TObject* pSender)
{
    // sending a print screen message, the entire windows can be copied to clipboard as bitmap
    keybd_event(VK_SNAPSHOT, 1, 0, 0);
}
//--------------------------------------------------------------------------------------------------
