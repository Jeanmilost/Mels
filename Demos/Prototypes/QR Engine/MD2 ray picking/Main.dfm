object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Marvin - MD2 ray picking'
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
    object miSeparator1: TMenuItem
      Caption = '-'
    end
    object miPrevAnim: TMenuItem
      Caption = 'Prev animation'
      ShortCut = 37
      OnClick = miPrevAnimClick
    end
    object miNextAnim: TMenuItem
      Caption = 'Next animation'
      ShortCut = 39
      OnClick = miNextAnimClick
    end
  end
end
