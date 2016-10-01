object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Shapes'
  ClientHeight = 527
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmOptions
  Position = poScreenCenter
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pmOptions: TPopupMenu
    Left = 552
    Top = 8
    object miLighting: TMenuItem
      Caption = 'Toggle light'
      ShortCut = 16460
      OnClick = miLightingClick
    end
  end
end
