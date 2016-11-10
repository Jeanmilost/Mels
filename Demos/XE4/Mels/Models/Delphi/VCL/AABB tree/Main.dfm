object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'MainForm'
  ClientHeight = 410
  ClientWidth = 410
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmOptions
  Position = poScreenCenter
  Scaled = False
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object paInfo: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 366
    Width = 390
    Height = 34
    Margins.Left = 10
    Margins.Top = 0
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alBottom
    BevelKind = bkSoft
    BevelOuter = bvNone
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 0
    object btRotate: TButton
      AlignWithMargins = True
      Left = 311
      Top = 0
      Width = 75
      Height = 30
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Action = acRotate
      Align = alRight
      Caption = 'Rotate'
      TabOrder = 0
    end
    object paCollisionResult: TPanel
      AlignWithMargins = True
      Left = 0
      Top = 8
      Width = 311
      Height = 13
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 9
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object laInCollision: TLabel
        AlignWithMargins = True
        Left = 134
        Top = 0
        Width = 62
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 15
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'In collision: 0'
        ExplicitLeft = 73
      end
      object laTotal: TLabel
        AlignWithMargins = True
        Left = 20
        Top = 0
        Width = 37
        Height = 13
        Margins.Left = 20
        Margins.Top = 0
        Margins.Right = 15
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'Total: 0'
        ExplicitLeft = 0
      end
      object laToTest: TLabel
        AlignWithMargins = True
        Left = 72
        Top = 0
        Width = 47
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 15
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'To test: 0'
        ExplicitLeft = 96
      end
      object laHighestHit: TLabel
        AlignWithMargins = True
        Left = 211
        Top = 0
        Width = 64
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 15
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'Highest hit: 0'
        ExplicitLeft = 226
      end
    end
  end
  object pmOptions: TPopupMenu
    Left = 360
    Top = 16
    object miRotateSphere: TMenuItem
      Action = acRotate
      Caption = 'Rotate sphere'
    end
  end
  object alRotate: TActionList
    Left = 320
    Top = 16
    object acRotate: TAction
      Category = 'Navigation'
      Caption = 'acRotate'
      ShortCut = 16466
      OnExecute = acRotateExecute
    end
  end
end
