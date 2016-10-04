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
#include <Vcl.Imaging.pngimage.hpp>
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
        TQRVCLBoxGL *boBox;
        TQRVCLSphereGL *spSphere;
        TQRVCLConeGL *coCone;
        TQRVCLTorusGL *toTorus;
        TQRVCLParabolaGL *prParabola;

    public:
        __fastcall TMainForm(TComponent* pOwner);
};
extern PACKAGE TMainForm* MainForm;
#endif
