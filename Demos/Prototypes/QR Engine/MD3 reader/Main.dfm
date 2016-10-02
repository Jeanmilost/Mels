object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Elf - MD3 renderer'
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
    object miSeparator1: TMenuItem
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
