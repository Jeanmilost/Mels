object Options: TOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Lara - Options'
  ClientHeight = 420
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
  PixelsPerInch = 96
  TextHeight = 13
  object paPreview: TPanel
    Left = 0
    Top = 0
    Width = 103
    Height = 385
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
    Height = 385
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbRenderOptions: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 125
      Width = 295
      Height = 92
      Margins.Left = 5
      Margins.Top = 25
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Render options'
      TabOrder = 1
      object ckShowCollisions: TCheckBox
        Left = 2
        Top = 32
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Show collisions with mouse pointer'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object ckUseShader: TCheckBox
        Left = 2
        Top = 49
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Use shader'
        Checked = True
        State = cbChecked
        TabOrder = 2
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
      object ckUseOrthoMatrix: TCheckBox
        Left = 2
        Top = 66
        Width = 291
        Height = 17
        Align = alTop
        Caption = 'Use orthogonal matrix'
        TabOrder = 3
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
    end
    object gbLoadModel: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 247
      Width = 295
      Height = 47
      Margins.Left = 5
      Margins.Top = 25
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Load model'
      TabOrder = 2
      object edModelFileName: TEdit
        Left = 3
        Top = 16
        Width = 246
        Height = 21
        ReadOnly = True
        TabOrder = 0
      end
      object btBrowse: TButton
        Left = 255
        Top = 16
        Width = 37
        Height = 21
        Caption = '...'
        TabOrder = 1
      end
    end
    object gbSelectTeam: TGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 324
      Width = 295
      Height = 44
      Margins.Left = 5
      Margins.Top = 25
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alTop
      Caption = 'Select team'
      TabOrder = 3
      object rbDefault: TRadioButton
        Left = 8
        Top = 16
        Width = 76
        Height = 17
        Caption = 'Default'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbRed: TRadioButton
        Left = 96
        Top = 16
        Width = 76
        Height = 17
        Caption = 'Red'
        TabOrder = 1
      end
      object rbBlue: TRadioButton
        Left = 184
        Top = 16
        Width = 76
        Height = 17
        Caption = 'Blue'
        TabOrder = 2
      end
    end
  end
  object paButtons: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 390
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
    end
  end
  object tiDrawPreview: TTimer
    Interval = 200
    Left = 32
    Top = 128
  end
  object odOpenDialog: TOpenDialog
    DefaultExt = 'pk3'
    Filter = 'MD3 model package|*.pk3|ZIP file|*.zip'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 32
    Top = 176
  end
end
