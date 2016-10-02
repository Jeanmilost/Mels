object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Marvin - MD2 renderer'
  ClientHeight = 527
  ClientWidth = 589
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmOptions
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pbLoadModel: TProgressBar
    Left = 0
    Top = 510
    Width = 589
    Height = 17
    Align = alBottom
    TabOrder = 0
  end
  object paRendering: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 510
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    DoubleBuffered = False
    FullRepaint = False
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object pmOptions: TPopupMenu
    Left = 552
    Top = 8
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
