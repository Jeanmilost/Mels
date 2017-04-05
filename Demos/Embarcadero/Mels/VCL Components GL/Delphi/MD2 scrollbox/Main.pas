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
 @abstract(@name contains the MD2 scrollbox demo main form.)
 @author(Jean-Milost Reymond)
 @created(2015 - 2017, this file is part of the Mels library)
}
unit Main;

interface

uses System.SysUtils,
     System.Variants,
     System.Classes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ComCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages,
     UTQRVCLModelComponentGL,
     UTQRVCLMD2ModelComponentGL;

type
    {**
     Main form
     @note Alpha blending should be enabled in the MD2 model to avoid flickering while the scrollbox
           is scrolled. In this coontext the alpha blending will behave the same way the
           DoubleBuffered behave for other controls
    }
    TMainForm = class(TForm)
        published
            sbMain: TScrollBox;
            laTitle: TLabel;
            m2Model: TQRVCLMD2ModelGL;
            reQuakeII: TRichEdit;
            btSaveToFile: TButton;
            sdSave: TSaveDialog;

            procedure FormShow(pSender: TObject);
            procedure btSaveToFileClick(pSender: TObject);

        private
            {**
            * Update rich edit height
            *}
            procedure UpdateRichEditHeight; virtual;
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
procedure TMainForm.FormShow(pSender: TObject);
begin
    if (m2Model.CanFocus) then
        m2Model.SetFocus;

    UpdateRichEditHeight;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btSaveToFileClick(pSender: TObject);
var
    pBitmap: Vcl.Graphics.TBitmap;
begin
    if (not sdSave.Execute) then
        Exit;

    pBitmap := nil;

    try
        // create a bitmap to receive the model
        pBitmap             := Vcl.Graphics.TBitmap.Create;
        pBitmap.PixelFormat := pf32bit;
        pBitmap.AlphaFormat := afPremultiplied;
        pBitmap.SetSize(100, 100);

        // draw model into bitmap
        SendMessage(m2Model.Handle, WM_PRINTCLIENT, LPARAM(pBitmap.Canvas.Handle), 0);

        // save bitmap
        pBitmap.SaveToFile(sdSave.FileName);
    finally
        pBitmap.Free;
    end;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.UpdateRichEditHeight();
var
    hDCt:                     HDC;
    hSaveFont:                HFONT;
    metrics:                  TTextMetric;
    lineHeight, increase, lc: Integer;
begin
    hDCt := GetDC(reQuakeII.Handle);

    try
        hSaveFont := SelectObject(hDCt, reQuakeII.Handle);
        GetTextMetrics(hDCt, metrics);
        SelectObject(hDCt, hSaveFont);
    finally
        ReleaseDC(reQuakeII.Handle, hDCt);
    end;

    lineHeight := metrics.tmHeight;
    increase   := reQuakeII.Height;
    lc         := reQuakeII.Lines.Count;

    if (lc < 1) then
        lc := 1;

    reQuakeII.Height        := (lc * lineHeight)        + 8;
    increase                := reQuakeII.Height        - increase;
    reQuakeII.Parent.Height := reQuakeII.Parent.Height + increase;
end;
//--------------------------------------------------------------------------------------------------

end.
