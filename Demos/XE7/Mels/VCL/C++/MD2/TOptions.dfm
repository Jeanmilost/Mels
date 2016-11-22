object Options: TOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Marvin - Options'
  ClientHeight = 381
  ClientWidth = 408
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object paPreview: TPanel
    Left = 0
    Top = 0
    Width = 103
    Height = 346
    Align = alLeft
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object imPreview: TImage
      Left = 0
      Top = 0
      Width = 103
      Height = 103
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Center = True
    end
  end
  object paMain: TPanel
    Left = 103
    Top = 0
    Width = 305
    Height = 346
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbLoadOptions: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 125
      Width = 295
      Height = 55
      Margins.Left = 5
      Margins.Top = 25
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Load options'
      TabOrder = 1
      object ckShowDefaultFrame: TCheckBox
        Left = 2
        Top = 15
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Show default frame'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object ckRunGestureWhenReady: TCheckBox
        Left = 2
        Top = 32
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Run selected gesture when fully loaded in cache'
        TabOrder = 1
      end
    end
    object gbRenderOptions: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 210
      Width = 295
      Height = 106
      Margins.Left = 5
      Margins.Top = 25
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Render options'
      TabOrder = 2
      object ckShowCollisions: TCheckBox
        Left = 2
        Top = 49
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Show collisions with mouse pointer'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object ckUseShader: TCheckBox
        Left = 2
        Top = 66
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Use shader'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
      object ckFullScreen: TCheckBox
        Left = 2
        Top = 15
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Full screen'
        TabOrder = 0
      end
      object ckPreCalculateLight: TCheckBox
        Left = 2
        Top = 32
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Pre-calculate light'
        TabOrder = 1
      end
      object ckUseOrthoMatrix: TCheckBox
        Left = 2
        Top = 83
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Use orthogonal matrix'
        TabOrder = 4
      end
    end
    object rgCacheOptions: TRadioGroup
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 295
      Height = 90
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Cache options'
      ItemIndex = 0
      Items.Strings = (
        'Create cache'
        'Create frames dynamically'
        'Create frames dynamically, cache created frames'
        'Custom')
      TabOrder = 0
      OnClick = rgCacheOptionsClick
    end
  end
  object paButtons: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 351
    Width = 398
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btOK: TButton
      AlignWithMargins = True
      Left = 323
      Top = 0
      Width = 75
      Height = 25
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btOKClick
    end
    object btCancel: TButton
      AlignWithMargins = True
      Left = 243
      Top = 0
      Width = 75
      Height = 25
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btCancelClick
    end
    object btQuit: TButton
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 113
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'Quit application'
      TabOrder = 2
      OnClick = btQuitClick
    end
  end
  object tiDrawPreview: TTimer
    Interval = 200
    OnTimer = tiDrawPreviewTimer
    Left = 32
    Top = 128
  end
end
