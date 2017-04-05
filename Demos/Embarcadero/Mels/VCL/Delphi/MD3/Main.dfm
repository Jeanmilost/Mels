object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Lara - MD3 renderer'
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
  Scaled = False
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
    Left = 535
    Top = 8
    object miPrevTorsoAnim: TMenuItem
      Caption = 'Prev torso animation'
      ShortCut = 37
      OnClick = miPrevTorsoAnimClick
    end
    object miNextTorsoAnim: TMenuItem
      Caption = 'Next torso animation'
      ShortCut = 39
      OnClick = miNextTorsoAnimClick
    end
    object miSeparator: TMenuItem
      Caption = '-'
    end
    object miPrevLegsAnim: TMenuItem
      Caption = 'Prev legs animation'
      ShortCut = 38
      OnClick = miPrevLegsAnimClick
    end
    object miNextLegsAnim: TMenuItem
      Caption = 'Next legs animation'
      ShortCut = 40
      OnClick = miNextLegsAnimClick
    end
  end
end
