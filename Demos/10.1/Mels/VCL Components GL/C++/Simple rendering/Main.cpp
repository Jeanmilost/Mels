/**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Simple renderer using Mels components demo main form                             *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************/

#include <vcl.h>
#pragma hdrstop
#include "Main.h"

// OpenGL
#include <GL/gl.h>

#pragma package(smart_init)
#pragma link "UTQRVCLModelComponentGL"
#pragma link "UTQRVCLSimpleRendererComponentGL"
#pragma resource "*.dfm"

//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
TMainForm* MainForm;
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* pOwner) :
    TForm(pOwner),
    m_hBlackBrush(NULL)
{
    m_hBlackBrush = (HBRUSH)::CreateSolidBrush(::ColorToRGB(clBlack));
}
//--------------------------------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
{
    if (m_hBlackBrush)
        ::DeleteObject(m_hBlackBrush);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnTopTextChange(TObject* pSender)
{
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckTopShowBelowTextClick(TObject* pSender)
{
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckTopShowAboveTextClick(TObject* pSender)
{
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckTopEnableTransparencyClick(TObject* pSender)
{
    srBlueSurface->AlphaBlending->Enabled = ckTopEnableTransparency->Checked;
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tbTopGlobalTransparencyChange(TObject* pSender)
{
    srBlueSurface->AlphaBlending->GlobalLevel = tbTopGlobalTransparency->Position;
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tbTopChangeColorAlphaChange(TObject* pSender)
{
    srBlueSurface->Color->Alpha = tbTopChangeColorAlpha->Position;
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::paTopChangeColorSelectClick(TObject* pSender)
{
    if (!cdColors->Execute())
        return;

    paTopChangeColorSelect->Color  = cdColors->Color;
    srBlueSurface->Color->VCLColor = cdColors->Color;
    srBlueSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::OnBottomTextChange(TObject* pSender)
{
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckBottomShowBelowTextClick(TObject* pSender)
{
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckBottomShowAboveTextClick(TObject* pSender)
{
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::ckBottomEnableTransparencyClick(TObject* pSender)
{
    srYellowSurface->AlphaBlending->Enabled = ckBottomEnableTransparency->Checked;
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tbBottomGlobalTransparencyChange(TObject* pSender)
{
    srYellowSurface->AlphaBlending->GlobalLevel = tbBottomGlobalTransparency->Position;
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::tbBottomChangeColorAlphaChange(TObject* pSender)
{
    srYellowSurface->Color->Alpha = tbBottomChangeColorAlpha->Position;
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::paBottomChangeColorSelectClick(TObject* pSender)
{
    if (!cdColors->Execute())
        return;

    paBottomChangeColorSelect->Color = cdColors->Color;
    srYellowSurface->Color->VCLColor = cdColors->Color;
    srYellowSurface->Invalidate();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srBlueSurfaceInitializeScene(TObject* pSender, NativeUInt hDC)
{
    if (!hDC)
        return;

    // is alpha blending enabled?
    if (!srBlueSurface->AlphaBlending->Enabled)
        return;

    // draw scene background
    DrawBg((HDC)hDC, srBlueSurface);

    // can show the text above the scene?
    if (ckTopShowBelowText->Checked)
    {
        // get text to draw and calculate text rect
        const std::wstring text = edTopBelowText->Text.c_str();
              TRect        textRect(10, 10, srBlueSurface->ClientWidth - 20, 30);

        // draw the text
        DrawText((HDC)hDC, text, textRect);
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srYellowSurfaceInitializeScene(TObject* pSender, NativeUInt hDC)
{
    if (!hDC)
        return;

    // is alpha blending enabled?
    if (!srYellowSurface->AlphaBlending->Enabled)
        return;

    // draw scene background
    DrawBg((HDC)hDC, srYellowSurface);

    // can show the text above the scene?
    if (ckBottomShowBelowText->Checked)
    {
        // get text to draw and calculate text rect
        const std::wstring text = edBottomBelowText->Text.c_str();
              TRect        textRect(10, 10, srYellowSurface->ClientWidth - 20, 30);

        // draw the text
        DrawText((HDC)hDC, text, textRect);
    }
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srBlueSurfaceDrawScene(TObject* pSender, NativeUInt hDC,
        NativeUInt hGLRC, TQRVCLModelRendererGL* pRenderer, TQRVCLModelShaderGL* pShader)
{
    DrawTriangle();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srYellowSurfaceDrawScene(TObject* pSender, NativeUInt hDC,
        NativeUInt hGLRC, TQRVCLModelRendererGL* pRenderer, TQRVCLModelShaderGL* pShader)
{
    DrawTriangle();
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srBlueSurfaceFinalizeScene(TObject *pSender, NativeUInt hDC)
{
    // can show the text above the scene?
    if (!ckTopShowAboveText->Checked)
        return;

    // get text to draw
    const std::wstring text = edTopAboveText->Text.c_str();

    // calculate text rectangle
    TRect textRect(10,
                   srBlueSurface->ClientHeight - 30,
                   srBlueSurface->ClientWidth  - 20,
                   srBlueSurface->ClientHeight - 10);

    // draw the text
    DrawText((HDC)hDC, text, textRect);
}
//--------------------------------------------------------------------------------------------------
void __fastcall TMainForm::srYellowSurfaceFinalizeScene(TObject *pSender, NativeUInt hDC)
{
    // can show the text above the scene?
    if (!ckBottomShowAboveText->Checked)
        return;

    // get text to draw
    const std::wstring text = edBottomAboveText->Text.c_str();

    // calculate text rectangle
    TRect textRect(10,
                   srYellowSurface->ClientHeight - 30,
                   srYellowSurface->ClientWidth  - 20,
                   srYellowSurface->ClientHeight - 10);

    // draw the text
    DrawText((HDC)hDC, text, textRect);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DrawBg(HDC hDC, TQRVCLSimpleRendererGL* pRenderer) const
{
    // no black brush?
    if (!m_hBlackBrush)
        return;

    TRect bgRect(0, 0, pRenderer->ClientWidth, pRenderer->ClientHeight);

    // fill scene background with black color
    ::FillRect((HDC)hDC, &bgRect, m_hBlackBrush);
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DrawTriangle() const
{
    glPushMatrix();

    glColor3f(0, 1, 1);

    // draw a simple RGB triangle demo
    glBegin(GL_TRIANGLES);
        glColor3f(1.0,0.0,0.0);
        glVertex3f( 0.0, 1.0, 0.0);
        glColor3f(0.0,1.0,0.0);
        glVertex3f(-1.0,-1.0, 0.0);
        glColor3f(0.0,0.0,1.0);
        glVertex3f( 1.0,-1.0, 0.0);
    glEnd();

    glPopMatrix();
}
//--------------------------------------------------------------------------------------------------
void TMainForm::DrawText(HDC hDC, const std::wstring& text, TRect& rect) const
{
    // configure GDI text color and background
    ::SetBkMode(hDC, TRANSPARENT);
    ::SetBkColor(hDC, ::ColorToRGB(clBlack));
    ::SetTextColor(hDC, ::ColorToRGB(clWhite));

    // draw the text on one line, truncate with ellipsis if exceeds the draw rect
    ::DrawTextW(hDC, text.c_str(), text.length(), &rect, DT_SINGLELINE | DT_END_ELLIPSIS);
}
//--------------------------------------------------------------------------------------------------
