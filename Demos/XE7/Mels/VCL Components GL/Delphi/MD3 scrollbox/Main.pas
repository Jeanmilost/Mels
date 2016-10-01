{**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : Main form for the scrollbox containing a MD3 model component demo.               *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit Main;

interface

uses Winapi.Windows,
     Winapi.Messages,
     System.SysUtils,
     System.Variants,
     System.Classes,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ComCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     UTQRVCLModelComponentGL,
     UTQRVCLMD3ModelComponentGL;

type
    {**
    * Main form
    *@note Alpha blending should be enabled in the MD2 model to avoid flickering while the scrollbox is
    *      scrolled. In this coontext the alpha blending will behave the same way the DoubleBuffered
    *      behave for other controls
    *}
    TMainForm = class(TForm)
        private
            {**
            * Update rich edit height
            *}
            procedure UpdateRichEditHeight; virtual;

        published
            sbMain: TScrollBox;
            laTitle: TLabel;
            reQuakeIII: TRichEdit;
            btSaveToFile: TButton;
            m3Model: TQRVCLMD3ModelGL;
            sdSave: TSaveDialog;

            procedure btSaveToFileClick(pSender: TObject);
            procedure FormShow(pSender: TObject);
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
    if (m3Model.CanFocus) then
        m3Model.SetFocus;

    UpdateRichEditHeight;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.btSaveToFileClick(pSender: TObject);
var
    pBitmap: TBitmap;
begin
    if (not sdSave.Execute) then
        Exit;

    pBitmap := nil;

    try
        // create a bitmap to receive the model
        pBitmap             := TBitmap.Create;
        pBitmap.PixelFormat := pf32bit;
        pBitmap.AlphaFormat := afPremultiplied;
        pBitmap.SetSize(100, 100);

        // draw model into bitmap
        SendMessage(m3Model.Handle, WM_PRINTCLIENT, LPARAM(pBitmap.Canvas.Handle), 0);

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
    hDCt := GetDC(reQuakeIII.Handle);

    try
        hSaveFont := SelectObject(hDCt, reQuakeIII.Handle);
        GetTextMetrics(hDCt, &metrics);
        SelectObject(hDCt, hSaveFont);
    finally
        ReleaseDC(reQuakeIII.Handle, hDCt);
    end;

    lineHeight := metrics.tmHeight;
    increase   := reQuakeIII.Height;
    lc         := reQuakeIII.Lines.Count;

    if (lc < 1) then
        lc := 1;

    reQuakeIII.Height        := (lc * lineHeight)        + 8;
    increase                 := reQuakeIII.Height        - increase;
    reQuakeIII.Parent.Height := reQuakeIII.Parent.Height + increase;
end;
//--------------------------------------------------------------------------------------------------

end.
