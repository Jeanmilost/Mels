/**************************************************************************************************
 * ==> TMainForm ---------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Threaded jobs demo main form                                                     *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// std
#include <string>
#include <memory>

#pragma package(smart_init)
#pragma link "UTQRThreading"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TMainForm::IKey
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::IKey::IKey() :
    m_Key(-1)
{}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::IKey::~IKey()
{}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_pWorker(NULL),
    m_Gen(0)
{
    // create and populate job worker
    m_pWorker             = new TQRVCLThreadWorker();
    m_pWorker->OnDone     = OnDone;
    m_pWorker->OnCanceled = OnCanceled;

    // create and populate cache (unfortunately cannot use the Mels cache from design patterns)
    m_Cache.Set_OnAdd(OnAdd);
    m_Cache.Set_OnDelete(OnDelete);
}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    // delete job worker
    if (m_pWorker)
    {
        m_pWorker->Cancel();
        delete m_pWorker;
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btAddClick(TObject* pSender)
{
    // create a new job and add it to worker
    std::auto_ptr<QR_DataSimulator> pData(new QR_DataSimulator());
    pData->Load(m_pWorker, 1000, 0x41 + (std::rand() % 0x19), 0x61 + (std::rand() % 0x19));
    pData.release();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::btDeleteClick(TObject* pSender)
{
    // get index to delete
    const int index = lbData->ItemIndex;

    // is index out of bounds?
    if (index < 0 || index >= lbData->Count)
        return;

    // delete data from cache
    m_Cache.Delete(static_cast<IKey*>(lbData->Items->Objects[index])->m_Key);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnDone(TQRThreadJob* pJob)
{
    // no job?
    if (!pJob)
        return;

    // job failed?
    if (pJob->GetStatus() == EQR_JS_Error)
    {
        // delete it
        delete pJob;
        return;
    }

    // create new key for data
    std::auto_ptr<IKey> pKey(new IKey());
    pKey->m_Key = m_Gen++;

    // get data to cache from job
    std::auto_ptr<QR_DataSimulator> pData(static_cast<QR_DataSimulator*>(pJob));

    // add data to cache
    if (!m_Cache.Add(pKey->m_Key, pData.get()))
        return;

    // show data
    lbData->Items->BeginUpdate();
    lbData->AddItem(pData->Get(), pKey.get());
    lbData->Items->EndUpdate();

    pKey.release();
    pData.release();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnCanceled(TQRThreadJob* pJob)
{
    // no job?
    if (!pJob)
        return;

    // delete canceled job
    delete pJob;
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnAdd(const std::size_t& key, QR_DataSimulator* const& value)
{}
//--------------------------------------------------------------------------------------------------
bool __fastcall TMainForm::OnDelete(const std::size_t& key, QR_DataSimulator*& value)
{
    // no value to delete?
    if (value)
        delete value;

    // search if data was shown on the interface, remove it if yes, and delete data
    for (int i = 0; i < lbData->Count; ++i)
        if (static_cast<IKey*>(lbData->Items->Objects[i])->m_Key == (int)key)
        {
            delete lbData->Items->Objects[i];
            lbData->Items->Delete(i);
        }

    return true;
}
//--------------------------------------------------------------------------------------------------
