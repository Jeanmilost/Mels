// *************************************************************************************************
// * ==> Main -------------------------------------------------------------------------------------*
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
#include "Main.h"

// std
#include <memory>

// resources
#include "Resources.rh"

#pragma package(smart_init)
#pragma link "UTQRPlayerAL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_Paused(false)
{}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject* pSender)
{
    // load sound from resources
    std::auto_ptr<TResourceStream> pStream(new TResourceStream((int)HInstance,
                                                               ID_DEFAULT_SOUND,
                                                               L"DATA"));

    // copy sound data to memory buffer
    std::auto_ptr<TMemoryStream> pSound(new TMemoryStream());
    pSound->CopyFrom(pStream.get(), pStream->Size);

    // load sound
    if (!plPlayer->Open((unsigned char*)pSound->Memory, pSound->Size))
    {
        MessageDlg(L"Could not load sound.\r\n\r\nApplication will close.",
                   mtError,
                   TMsgDlgButtons() << mbOK,
                   0);

        Application->Terminate();
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btPlayClick(TObject* pSender)
{
    btPause->Enabled = true;
    btStop->Enabled  = true;
    m_Paused         = false;

    plPlayer->Play();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btPauseClick(TObject* pSender)
{
    if (plPlayer->IsPlaying())
    {
        m_Paused = true;
        plPlayer->Pause();
    }
    else
    {
        m_Paused = false;
        plPlayer->Play();
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btStopClick(TObject* pSender)
{
    btPause->Enabled = false;
    btStop->Enabled  = false;
    m_Paused         = false;

    plPlayer->Stop();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btSelectClick(TObject* pSender)
{
    if (!odOpen->Execute())
        return;

    if (::ExtractFileExt(odOpen->FileName) != L".wav")
    {
        MessageDlg(L"File is not a wav sound.",
                   mtError,
                   TMsgDlgButtons() << mbOK,
                   0);

        return;
    }

    // open sound file
    std::auto_ptr<TFileStream> pFile(new TFileStream(odOpen->FileName, fmOpenRead));
    std::auto_ptr<TMemoryStream> pSound(new TMemoryStream());
    pSound->CopyFrom(pFile.get(), pFile->Size);

    // load sound
    if (!plPlayer->Open((unsigned char*)pSound->Memory, pSound->Size))
    {
        MessageDlg(L"Could not load sound.",
                   mtError,
                   TMsgDlgButtons() << mbOK,
                   0);

        return;
    }

    edFileName->Text = odOpen->FileName;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tiTimerTimer(TObject* pSender)
{
    if (plPlayer->IsPlaying() || m_Paused)
        return;

    btPause->Enabled = false;
    btStop->Enabled  = false;
}
//--------------------------------------------------------------------------------------------------
