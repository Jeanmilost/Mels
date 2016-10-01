/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Shapes demo main form                                                            *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#ifndef MainH
#define MainH

// vcl
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

// Mels library
#include <UTQRVCLModelComponentGL.hpp>
#include <UTQRVCLShapeComponentGL.hpp>

/**
* Shapes demo
*/
class TMainForm : public TForm
{
    __published:
        TQRVCLSurfaceGL *suSurface;
        TTimer *tiAnimation;
        TQRVCLBoxGL *boBox;
        TQRVCLSphereGL *spSphere;
        TQRVCLConeGL *coCone;
        TQRVCLTorusGL *toTorus;
        TQRVCLParabolaGL *prParabola;

        void __fastcall tiAnimationTimer(TObject* pSender);

    public:
        __fastcall TMainForm(TComponent* pOwner);

    private:
        float m_Angle;
};
extern PACKAGE TMainForm* MainForm;
#endif
