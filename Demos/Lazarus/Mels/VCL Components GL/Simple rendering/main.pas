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

{**
 @abstract(@name contains the simple rendering demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2016, this file is part of the Mels library)
}
unit Main;

{$MODE Delphi}

interface

uses Classes,
     SysUtils,
     Variants,
     Graphics,
     Controls,
     ComCtrls,
     StdCtrls,
     ExtCtrls,
     Forms,
     Dialogs,
     Windows,
     Gl,
     UTQRVCLModelRendererGL,
     UTQRVCLModelShaderGL,
     UTQRVCLModelComponentGL,
     UTQRVCLSimpleRendererComponentGL;

type
    {**
     Main form
    }
    TMainForm = class(TForm)
        published
            cdColors: TColorDialog;
            paBlueScene: TPanel;
            spTop: TSplitter;
            srBlueSurface: TQRVCLSimpleRendererGL;
            paTopControls: TPanel;
            laTopBelowText: TLabel;
            laTopAboveText: TLabel;
            laTopChangeGlobalTransparency: TLabel;
            edTopBelowText: TEdit;
            edTopAboveText: TEdit;
            ckTopEnableTransparency: TCheckBox;
            ckTopShowBelowText: TCheckBox;
            ckTopShowAboveText: TCheckBox;
            tbTopGlobalTransparency: TTrackBar;
            paTopChangeColor: TPanel;
            laTopChangeColor: TLabel;
            paTopChangeColorSelect: TPanel;
            paTopChangeColorAlpha: TPanel;
            laTopChangeColorAlpha: TLabel;
            tbTopChangeColorAlpha: TTrackBar;
            paYellowScene: TPanel;
            spBottom: TSplitter;
            srYellowSurface: TQRVCLSimpleRendererGL;
            paBottomControls: TPanel;
            laBottomAboveText: TLabel;
            laBottomBelowText: TLabel;
            laBottomChangeGlobalTransparency: TLabel;
            ckBottomEnableTransparency: TCheckBox;
            ckBottomShowAboveText: TCheckBox;
            ckBottomShowBelowText: TCheckBox;
            edBottomAboveText: TEdit;
            edBottomBelowText: TEdit;
            paBottomChangeColor: TPanel;
            laBottomChangeColor: TLabel;
            paBottomChangeColorSelect: TPanel;
            paBottomChangeColorAlpha: TPanel;
            laBottomChangeColorAlpha: TLabel;
            tbBottomChangeColorAlpha: TTrackBar;
            tbBottomGlobalTransparency: TTrackBar;
            spScenes: TSplitter;

            procedure OnTopTextChange(pSender: TObject);
            procedure ckTopShowBelowTextClick(pSender: TObject);
            procedure ckTopShowAboveTextClick(pSender: TObject);
            procedure ckTopEnableTransparencyClick(pSender: TObject);
            procedure tbTopGlobalTransparencyChange(pSender: TObject);
            procedure tbTopChangeColorAlphaChange(pSender: TObject);
            procedure paTopChangeColorSelectClick(pSender: TObject);
            procedure OnBottomTextChange(pSender: TObject);
            procedure ckBottomShowBelowTextClick(pSender: TObject);
            procedure ckBottomShowAboveTextClick(pSender: TObject);
            procedure ckBottomEnableTransparencyClick(pSender: TObject);
            procedure tbBottomGlobalTransparencyChange(pSender: TObject);
            procedure tbBottomChangeColorAlphaChange(pSender: TObject);
            procedure paBottomChangeColorSelectClick(pSender: TObject);
            procedure srBlueSurfaceInitializeScene(pSender: TObject; hDC: NativeUInt);
            procedure srYellowSurfaceInitializeScene(pSender: TObject; hDC: NativeUInt);
            procedure srBlueSurfaceDrawScene(pSender: TObject;
                                          hDC, hGLRC: NativeUInt;
                                           pRenderer: TQRVCLModelRendererGL;
                                             pShader: TQRVCLModelShaderGL);
            procedure srYellowSurfaceDrawScene(pSender: TObject;
                                            hDC, hGLRC: NativeUInt;
                                             pRenderer: TQRVCLModelRendererGL;
                                               pShader: TQRVCLModelShaderGL);
            procedure srBlueSurfaceFinalizeScene(pSender: TObject; hDC: NativeUInt);
            procedure srYellowSurfaceFinalizeScene(pSender: TObject; hDC: NativeUInt);

        private
            m_hBlackBrush: HBRUSH;

            {**
             Draws the scene background
             @param(hDC Device context to draw on)
             @param(pRenderer Surface renderer control)
            }
            procedure DrawBg(hDC: THandle; pRenderer: TQRVCLSimpleRendererGL);

            {**
             Draws a triangle covering the entire scene area using OpenGL
            }
            procedure DrawTriangle;

            {**
             Draws text on a device context
             @param(hDC Device context to draw on)
             @param(text Text to draw)
             @param(rect @bold([in, out]) Rectangle bounding text to draw)
            }
            procedure DrawText(hDC: THandle; const text: UnicodeString; rect: TRect);

        public
            {**
             Constructor
             @param (pOwner Form owner)
            }
            constructor Create(pOwner: TComponent); override;

            {**
             Destructor
            }
            destructor Destroy; override;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// Resources
//--------------------------------------------------------------------------------------------------
{$R *.lfm}
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    // create the global brush to use to paint background in the whole interface
    m_hBlackBrush := CreateSolidBrush(ColorToRGB(clBlack));
end;
//--------------------------------------------------------------------------------------------------
destructor TMainForm.Destroy;
begin
    if (m_hBlackBrush <> 0) then
        DeleteObject(m_hBlackBrush);

    inherited Destroy;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnTopTextChange(pSender: TObject);
begin
    srBlueSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckTopShowBelowTextClick(pSender: TObject);
begin
    srBlueSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckTopShowAboveTextClick(pSender: TObject);
begin
    srBlueSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckTopEnableTransparencyClick(pSender: TObject);
begin
    srBlueSurface.AlphaBlending.Enabled := ckTopEnableTransparency.Checked;
    srBlueSurface.Invalidate();
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tbTopGlobalTransparencyChange(pSender: TObject);
begin
    srBlueSurface.AlphaBlending.GlobalLevel := tbTopGlobalTransparency.Position;
    srBlueSurface.Invalidate();
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tbTopChangeColorAlphaChange(pSender: TObject);
begin
    srBlueSurface.Color.Alpha := tbTopChangeColorAlpha.Position;
    srBlueSurface.Invalidate();
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.paTopChangeColorSelectClick(pSender: TObject);
begin
    if (not cdColors.Execute) then
        Exit;

    paTopChangeColorSelect.Color := cdColors.Color;
    srBlueSurface.Color.VCLColor := cdColors.Color;
    srBlueSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.OnBottomTextChange(pSender: TObject);
begin
    srYellowSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckBottomShowBelowTextClick(pSender: TObject);
begin
    srYellowSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckBottomShowAboveTextClick(pSender: TObject);
begin
    srYellowSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.ckBottomEnableTransparencyClick(pSender: TObject);
begin
    srYellowSurface.AlphaBlending.Enabled := ckBottomEnableTransparency.Checked;
    srYellowSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tbBottomGlobalTransparencyChange(pSender: TObject);
begin
    srYellowSurface.AlphaBlending.GlobalLevel := tbBottomGlobalTransparency.Position;
    srYellowSurface.Invalidate();
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tbBottomChangeColorAlphaChange(pSender: TObject);
begin
    srYellowSurface.Color.Alpha := tbBottomChangeColorAlpha.Position;
    srYellowSurface.Invalidate();
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.paBottomChangeColorSelectClick(pSender: TObject);
begin
    if (not cdColors.Execute) then
        Exit;

    paBottomChangeColorSelect.Color := cdColors.Color;
    srYellowSurface.Color.VCLColor  := cdColors.Color;
    srYellowSurface.Invalidate;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srBlueSurfaceInitializeScene(pSender: TObject; hDC: NativeUInt);
var
    textRect: TRect;
begin
    if (hDC = 0) then
        Exit;

    // is alpha blending enabled?
    if (not srBlueSurface.AlphaBlending.Enabled) then
        Exit;

    // draw scene background
    DrawBg(hDC, srBlueSurface);

    // can show the text above the scene?
    if (ckTopShowBelowText.Checked) then
    begin
        // calculate text rect
        textRect.Left   := 10;
        textRect.Top    := 10;
        textRect.Right  := srBlueSurface.ClientWidth - 20;
        textRect.Bottom := 30;

        // draw the text
        DrawText(hDC, UnicodeString(edTopBelowText.Text), textRect);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srYellowSurfaceInitializeScene(pSender: TObject; hDC: NativeUInt);
var
    textRect: TRect;
begin
    if (hDC = 0) then
        Exit;

    // is alpha blending enabled?
    if (not srYellowSurface.AlphaBlending.Enabled) then
        Exit;

    // draw scene background
    DrawBg(hDC, srYellowSurface);

    // can show the text above the scene?
    if (ckBottomShowBelowText.Checked) then
    begin
        // calculate text rect
        textRect.Left   := 10;
        textRect.Top    := 10;
        textRect.Right  := srYellowSurface.ClientWidth - 20;
        textRect.Bottom := 30;

        // draw the text
        DrawText(hDC, UnicodeString(edBottomBelowText.Text), textRect);
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srBlueSurfaceDrawScene(pSender: TObject;
                                        hDC, hGLRC: NativeUInt;
                                         pRenderer: TQRVCLModelRendererGL;
                                           pShader: TQRVCLModelShaderGL);
begin
    DrawTriangle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srYellowSurfaceDrawScene(pSender: TObject;
                                          hDC, hGLRC: NativeUInt;
                                           pRenderer: TQRVCLModelRendererGL;
                                             pShader: TQRVCLModelShaderGL);
begin
    DrawTriangle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srBlueSurfaceFinalizeScene(pSender: TObject; hDC: NativeUInt);
var
    textRect:TRect;
begin
    // can show the text above the scene?
    if (not ckTopShowAboveText.Checked) then
        Exit;

    // calculate text rectangle
    textRect.Left   := 10;
    textRect.Top    := srBlueSurface.ClientHeight - 30;
    textRect.Right  := srBlueSurface.ClientWidth  - 20;
    textRect.Bottom := srBlueSurface.ClientHeight - 10;

    // draw the text
    DrawText(hDC, UnicodeString(edTopAboveText.Text), textRect);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.srYellowSurfaceFinalizeScene(pSender: TObject; hDC: NativeUInt);
var
    textRect: TRect;
begin
    // can show the text above the scene?
    if (not ckBottomShowAboveText.Checked) then
        Exit;

    // calculate text rectangle
    textRect.Left   := 10;
    textRect.Top    := srYellowSurface.ClientHeight - 30;
    textRect.Right  := srYellowSurface.ClientWidth  - 20;
    textRect.Bottom := srYellowSurface.ClientHeight - 10;

    // draw the text
    DrawText(hDC, UnicodeString(edBottomAboveText.Text), textRect);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DrawBg(hDC: THandle; pRenderer: TQRVCLSimpleRendererGL);
var
    bgRect: TRect;
begin
    // no black brush?
    if (m_hBlackBrush = 0) then
        Exit;

    bgRect.Left   := 0;
    bgRect.Top    := 0;
    bgRect.Right  := pRenderer.ClientWidth;
    bgRect.Bottom := pRenderer.ClientHeight;

    // fill scene background with black color
    FillRect(hDC, bgRect, m_hBlackBrush);
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DrawTriangle;
begin
    glPushMatrix;

    glColor3f(0, 1, 1);

    // draw a simple RGB triangle demo
    glBegin(GL_TRIANGLES);
        glColor3f(1.0,0.0,0.0);
        glVertex3f( 0.0, 1.0, 0.0);
        glColor3f(0.0,1.0,0.0);
        glVertex3f(-1.0,-1.0, 0.0);
        glColor3f(0.0,0.0,1.0);
        glVertex3f( 1.0,-1.0, 0.0);
    glEnd;

    glPopMatrix;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.DrawText(hDC: THandle; const text: UnicodeString; rect: TRect);
begin
    // configure GDI text color and background
    SetBkMode(hDC, TRANSPARENT);
    SetBkColor(hDC, ColorToRGB(clBlack));
    SetTextColor(hDC, ColorToRGB(clWhite));

    // draw the text on one line, truncate with ellipsis if exceeds the draw rect
    DrawTextW(hDC, PWideChar(text), Length(text), rect, DT_SINGLELINE or DT_END_ELLIPSIS);
end;
//--------------------------------------------------------------------------------------------------

end.
