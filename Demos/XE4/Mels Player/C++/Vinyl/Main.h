/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Main form for the vinyl demo.                                                    *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Imaging.jpeg.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>

// Mels library
#include <UTQRPlayerAL.hpp>
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLShapeComponentGL.hpp>

/**
* Main form for the vinyl demo
*/
class TMainForm : public TForm
{
    __published:
        TQRVCLSurfaceGL *suVinyl;
        TTimer *tiAnimation;
        TQRPlayerAL *mpPlayer;
        TImage *imStart;
        TImage *imPause;
        TImage *imStop;

        void __fastcall tiAnimationTimer(TObject* pSender);
        void __fastcall imStartClick(TObject* pSender);
        void __fastcall imPauseClick(TObject* pSender);
        void __fastcall imStopClick(TObject* pSender);

    public:
        __fastcall TMainForm(TComponent* pOwner);

    private:
        float m_Angle;
};
extern PACKAGE TMainForm* MainForm;
#endif
