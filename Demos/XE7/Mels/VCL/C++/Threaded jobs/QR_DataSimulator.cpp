/**************************************************************************************************
 * ==> QR_DataSimulator --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Simple demo data simulator                                                       *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include "QR_DataSimulator.h"

// libraries
#include <Windows.h>

#pragma link "UTQRThreading"

//--------------------------------------------------------------------------------------------------
// QR_DataSimulator
//--------------------------------------------------------------------------------------------------
__fastcall QR_DataSimulator::QR_DataSimulator() :
    TQRVCLThreadWorkerJob(),
    m_Length(0),
    m_High(0x0),
    m_Low(0x0),
    m_Canceled(false),
    m_pData(NULL)
{}
//--------------------------------------------------------------------------------------------------
__fastcall QR_DataSimulator::~QR_DataSimulator()
{
    Delete();
}
//--------------------------------------------------------------------------------------------------
bool QR_DataSimulator::Load(TQRVCLThreadWorker* pWorker, std::size_t length, char high, char low)
{
    if (!pWorker)
        return false;

    // lock thread, and access data only when entering in critical section is allowed
    m_pLock->Lock();
    m_Length = length;
    m_High   = high;
    m_Low    = low;
    m_pLock->Unlock();

    pWorker->AddJob(this);
    return true;
}
//--------------------------------------------------------------------------------------------------
bool QR_DataSimulator::Load(std::size_t length, char high, char low)
{
    Delete();

    if (!length)
        return true;

    m_pLock->Lock();
    m_pData = new char[length];
    m_pLock->Unlock();

    for (std::size_t i = 0; i < length; ++i)
    {
        bool canceled;

        m_pLock->Lock();
        m_pData[i] = (i % 2) ? high : low;
        canceled   = m_Canceled;
        m_pLock->Unlock();

        if (canceled)
            break;

        // to simulate a very heavy task (i.e. that takes many time to be processed)
        ::Sleep(10);
    }

    return true;
}
//--------------------------------------------------------------------------------------------------
void QR_DataSimulator::Delete()
{
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock->Lock();

    try
    {
        if (!m_pData)
            return;

        delete[] m_pData;
        m_pData = NULL;
    }
    __finally
    {
        m_pLock->Unlock();
    }
}
//--------------------------------------------------------------------------------------------------
char* QR_DataSimulator::Get()
{
    char* pData;

    // lock thread, and access data only when entering in critical section is allowed
    m_pLock->Lock();
    pData = m_pData;
    m_pLock->Unlock();

    return pData;
}
//--------------------------------------------------------------------------------------------------
bool __fastcall QR_DataSimulator::Process()
{
    return Load(m_Length, m_High, m_Low);
}
//--------------------------------------------------------------------------------------------------
void __fastcall QR_DataSimulator::Cancel()
{
    // lock thread, and access data only when entering in critical section is allowed
    m_pLock->Lock();
    m_Canceled = true;
    m_pLock->Unlock();
}
//--------------------------------------------------------------------------------------------------
