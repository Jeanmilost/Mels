/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Shapes demo main form                                                            *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

#pragma package(smart_init)
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLShapeComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
TMainForm *MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_Angle(0.0f)
{
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tiAnimationTimer(TObject *Sender)
{
    suSurface->Model->RotationZ = m_Angle;

    m_Angle += 0.05f;

    if (m_Angle > M_PI * 2.0)
        m_Angle -= M_PI * 2.0;

    suSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
