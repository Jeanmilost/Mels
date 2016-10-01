/**************************************************************************************************
 * ==> QR_DataSimulator --------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Simple demo data simulator                                                       *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef QR_DataSimulatorH
#define QR_DataSimulatorH

// std
#include <string>

// Mels library
#include "UTQRThreading.hpp"

/**
* Data simulator, it's a demo class that generates random data
*@author Jean-Milost Reymond
*/
class QR_DataSimulator : public TQRVCLThreadWorkerJob
{
    public:
        __fastcall QR_DataSimulator();
        virtual __fastcall ~QR_DataSimulator();

        /**
        * Loads data
        *@param pWorker - worker that will do the job
        *@param length - data length to generate
        *@param high - alternate upper-case char to set
        *@param low - alternate lower-case char to set
        *@return true on success, otherwise false
        */
        virtual bool Load(TQRVCLThreadWorker* pWorker, std::size_t length, char high, char low);

        /**
        * Loads data
        *@param length - data length to generate
        *@param high - alternate upper-case char to set
        *@param low - alternate lower-case char to set
        *@return true on success, otherwise false
        */
        virtual bool Load(std::size_t length, char high, char low);

        /**
        * Deletes data
        */
        virtual void Delete();

        /**
        * Gets data
        *@return data
        */
        virtual char* Get();

        /**
        * Processes the job
        *@returns true on success, otherwise false
        */
        virtual bool __fastcall Process();

        /**
        * Cancels the job
        */
        virtual void __fastcall Cancel();

    private:
        std::size_t m_Length;
        char        m_High;
        char        m_Low;
        char*       m_pData;
        bool        m_Canceled;
};

#endif
