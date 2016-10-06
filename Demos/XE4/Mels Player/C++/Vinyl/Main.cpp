/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Main form for the vinyl demo.                                                    *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

#include <memory>

#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRPlayerAL"
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLShapeComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_Angle(M_PI * 2.0f)
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // get music from resources
    std::auto_ptr<TResourceStream> pVertexPrg(new TResourceStream((int)HInstance, ID_MUSIC, L"DATA"));
    std::auto_ptr<TMemoryStream>   pBuffer(new TMemoryStream());
    pBuffer->CopyFrom(pVertexPrg.get(), pVertexPrg->Size);

    // load music
    if (!mpPlayer->Open((unsigned char*)pBuffer->Memory, pBuffer->Size))
    {
        ::MessageDlg("Could not load music.\r\n\r\nApplication will close.",
                     mtError,
                     TMsgDlgButtons() << mbOK,
                     0);

        Application->Terminate();
        return;
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::imStartClick(TObject* pSender)
{
    mpPlayer->Play();
    tiAnimation->Enabled = true;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::imPauseClick(TObject* pSender)
{
    if (mpPlayer->IsPlaying())
        mpPlayer->Pause();
    else
        mpPlayer->Play();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::imStopClick(TObject* pSender)
{
    mpPlayer->Stop();
    tiAnimation->Enabled = false;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tiAnimationTimer(TObject* pSender)
{
    m_Angle -= 0.1f;

    if (m_Angle <= 0.0f)
        m_Angle += M_PI * 2.0f;

    suVinyl->Model->RotationZ = m_Angle;

    suVinyl->Invalidate();
}
//--------------------------------------------------------------------------------------------------
