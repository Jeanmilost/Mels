/**************************************************************************************************
 * ==> TMainForm ---------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Threaded jobs demo main form                                                     *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef MainH
#define MainH

#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>

// qr engine
#include "QR_Cache.h"
#include "QR_DataSimulator.h"

// Mels library
#include <UTQRThreading.hpp>

/**
* Threaded jobs demo main form
*@author Jean-Milost Reymond
*/
class TMainForm : public TForm
{
    __published:
        TButton *btAdd;
        TPanel *paButtons;
        TButton *btDelete;
        TListBox *lbData;

        void __fastcall btAddClick(TObject* pSender);
        void __fastcall btDeleteClick(TObject* pSender);

    public:
        /**
        * Constructor
        *@param pOwner - form owner
        */
        __fastcall TMainForm(TComponent* pOwner);

        virtual __fastcall ~TMainForm();

    private:
        /**
        * Cached data key
        */
        class IKey : public TObject
        {
            public:
                int m_Key;

                virtual __fastcall IKey();
                virtual __fastcall ~IKey();
        };

        QR_Cache<std::size_t, QR_DataSimulator*> m_Cache;
        TQRVCLThreadWorker*                      m_pWorker;
        std::size_t                              m_Gen;

        /**
        * Called when a job is done
        *@param pJob - done job
        */
        void __fastcall OnDone(TQRThreadJob* pJob);

        /**
        * Called when a job is canceled
        *@param pJob - done job
        */
        void __fastcall OnCanceled(TQRThreadJob* pJob);

        /**
        * Called when new value is added to cache
        *@param key - newly added key
        *@param value - newly added avlue
        */
        void __fastcall OnAdd(const std::size_t& key, QR_DataSimulator* const& value);

        /**
        * Called when value is deleted from cache
        *@param key - deleting key
        *@param[in, out] value - deleting value
        *@return true if value can be deleted from cache, otherwise false
        */
        bool __fastcall OnDelete(const std::size_t& key, QR_DataSimulator*& value);
};
extern PACKAGE TMainForm* MainForm;
#endif
