{**************************************************************************************************
 * ==> Main --------------------------------------------------------------------------------------*
 **************************************************************************************************
 * Description : MD2 models using Mels components demo main form                                  *
 * Developer   : Jean-Milost Reymond                                                              *
 * Copyright   : 2015 - 2016, this file is part of the Mels library, all right reserved           *
 **************************************************************************************************}

unit Main;

interface

uses System.SysUtils,
     System.Variants,
     System.Classes,
     System.Actions,
     Vcl.ActnList,
     Vcl.Graphics,
     Vcl.Controls,
     Vcl.StdCtrls,
     Vcl.ExtCtrls,
     Vcl.Forms,
     Vcl.Dialogs,
     Winapi.Windows,
     Winapi.Messages,
     UTQRVCLModelComponentGL,
     UTQRVCLMD2ModelComponentGL;

type
    {**
    * MD2 demo main form
    *@author Jean-Milost Reymond
    *}
    TMainForm = class(TForm)
        private
            m_MiddleLeftAngle: Single;

        public
            constructor Create(pOwner: TComponent); override;

        published
            alActions: TActionList;
            acCopyToClipboard: TAction;
            paModelCaptionsBottom: TPanel;
            laModelCaptionBottomLeft: TLabel;
            laModelCaptionBottomMiddle: TLabel;
            laModelCaptionBottomRight: TLabel;
            paModelCaptionsMiddle: TPanel;
            laModeCaptionlMiddleLeft: TLabel;
            laModelCaptionMiddleMiddle: TLabel;
            laModelCaptionMiddleRight: TLabel;
            paModelCaptionsTop: TPanel;
            laModelCaptionTopLeft: TLabel;
            laModelCaptionTopMiddle: TLabel;
            laModelCaptionTopRight: TLabel;
            paModelsBottom: TPanel;
            m2ModelBottomMiddle: TQRVCLMD2ModelGL;
            m2ModelBottomLeft: TQRVCLMD2ModelGL;
            m2ModelBottomRight: TQRVCLMD2ModelGL;
            paModelsMiddle: TPanel;
            m2ModelMiddleMiddle: TQRVCLMD2ModelGL;
            m2ModelMiddleLeft: TQRVCLMD2ModelGL;
            m2ModelMiddleRight: TQRVCLMD2ModelGL;
            paModelsTop: TPanel;
            m2ModelTopLeft: TQRVCLMD2ModelGL;
            m2ModelTopRight: TQRVCLMD2ModelGL;
            m2ModelTopMiddle: TQRVCLMD2ModelGL;
            tiAnimate: TTimer;

            procedure tiAnimateTimer(pSender: TObject);
            procedure acCopyToClipboardExecute(pSender: TObject);
    end;

var
    MainForm: TMainForm;

implementation
//--------------------------------------------------------------------------------------------------
// TMainForm
//--------------------------------------------------------------------------------------------------
{$R *.dfm}
//--------------------------------------------------------------------------------------------------
constructor TMainForm.Create(pOwner: TComponent);
begin
    inherited Create(pOwner);

    m_MiddleLeftAngle := -0.7854;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.acCopyToClipboardExecute(pSender: TObject);
begin
    // calculate next angle
    m_MiddleLeftAngle := m_MiddleLeftAngle + 0.05;

    // is angle exceeding the max limit?
    if (m_MiddleLeftAngle >= PI * 2.0) then
        m_MiddleLeftAngle := m_MiddleLeftAngle - (PI * 2.0);

    // apply rotation to model
    m2ModelMiddleLeft.Model.RotationY := m_MiddleLeftAngle;
end;
//--------------------------------------------------------------------------------------------------
procedure TMainForm.tiAnimateTimer(pSender: TObject);
begin
    // sending a print screen message, the entire windows can be copied to clipboard as bitmap
    keybd_event(VK_SNAPSHOT, 1, 0, 0);
end;
//--------------------------------------------------------------------------------------------------

end.
