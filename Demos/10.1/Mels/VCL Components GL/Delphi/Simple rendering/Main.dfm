object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Simple rendering surface'
  ClientHeight = 500
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object spScenes: TSplitter
    Left = 0
    Top = 250
    Width = 589
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitLeft = -8
    ExplicitTop = 253
  end
  object paBlueScene: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 250
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object spTop: TSplitter
      Left = 250
      Top = 0
      Height = 250
      ExplicitLeft = 376
      ExplicitTop = 96
      ExplicitHeight = 100
    end
    object srBlueSurface: TQRVCLSimpleRendererGL
      Left = 0
      Top = 0
      Width = 250
      Height = 250
      Color.Red = 0
      Color.Green = 0
      Color.Blue = 255
      Color.Alpha = 128
      Color.VCLColor = clBlue
      Color.WinColor = 16711680
      AlphaBlending.Enabled = True
      OnInitializeScene = srBlueSurfaceInitializeScene
      OnFinalizeScene = srBlueSurfaceFinalizeScene
      Align = alLeft
      OnDrawScene = srBlueSurfaceDrawScene
    end
    object paTopControls: TPanel
      Left = 253
      Top = 0
      Width = 336
      Height = 250
      Align = alClient
      TabOrder = 1
      object laTopBelowText: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 328
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Text below the scene'
        ExplicitWidth = 103
      end
      object laTopAboveText: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 328
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Text above the scene'
        ExplicitWidth = 105
      end
      object laTopChangeGlobalTransparency: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 144
        Width = 328
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Change global transparency'
        ExplicitWidth = 135
      end
      object edTopBelowText: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 20
        Width = 328
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        TabOrder = 0
        Text = 'This text is written below the scene'
        OnChange = OnTopTextChange
      end
      object edTopAboveText: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 60
        Width = 328
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        TabOrder = 1
        Text = 'This text is written above the scene'
        OnChange = OnTopTextChange
      end
      object ckTopEnableTransparency: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 124
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Enable alpha transparency'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = ckTopEnableTransparencyClick
      end
      object ckTopShowBelowText: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 84
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Show below text'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = ckTopShowBelowTextClick
      end
      object ckTopShowAboveText: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 104
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Show above text'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = ckTopShowAboveTextClick
      end
      object tbTopGlobalTransparency: TTrackBar
        AlignWithMargins = True
        Left = 4
        Top = 160
        Width = 328
        Height = 45
        Margins.Bottom = 0
        Align = alTop
        Max = 255
        Frequency = 25
        Position = 255
        TabOrder = 5
        OnChange = tbTopGlobalTransparencyChange
      end
      object paTopChangeColor: TPanel
        Left = 1
        Top = 205
        Width = 334
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 6
        object laTopChangeColor: TLabel
          Left = 0
          Top = 0
          Width = 94
          Height = 40
          Align = alLeft
          Caption = 'Change scene color'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object paTopChangeColorSelect: TPanel
          AlignWithMargins = True
          Left = 299
          Top = 3
          Width = 32
          Height = 34
          Cursor = crHandPoint
          Align = alRight
          BevelOuter = bvNone
          Color = clBlue
          ParentBackground = False
          TabOrder = 1
          OnClick = paTopChangeColorSelectClick
        end
        object paTopChangeColorAlpha: TPanel
          Left = 168
          Top = 0
          Width = 128
          Height = 40
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object laTopChangeColorAlpha: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 122
            Height = 13
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Alpha'
            ExplicitWidth = 27
          end
          object tbTopChangeColorAlpha: TTrackBar
            AlignWithMargins = True
            Left = 3
            Top = 19
            Width = 122
            Height = 20
            Margins.Bottom = 0
            Align = alTop
            Max = 255
            Frequency = 25
            Position = 128
            ShowSelRange = False
            TabOrder = 0
            OnChange = tbTopChangeColorAlphaChange
          end
        end
      end
    end
  end
  object paYellowScene: TPanel
    Left = 0
    Top = 253
    Width = 589
    Height = 247
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object spBottom: TSplitter
      Left = 250
      Top = 0
      Height = 247
      ExplicitTop = 500
    end
    object srYellowSurface: TQRVCLSimpleRendererGL
      Left = 0
      Top = 0
      Width = 250
      Height = 247
      Color.Red = 255
      Color.Green = 255
      Color.Blue = 0
      Color.Alpha = 64
      Color.VCLColor = clYellow
      Color.WinColor = 65535
      AlphaBlending.Enabled = True
      OnInitializeScene = srYellowSurfaceInitializeScene
      OnFinalizeScene = srYellowSurfaceFinalizeScene
      Align = alLeft
      OnDrawScene = srYellowSurfaceDrawScene
    end
    object paBottomControls: TPanel
      Left = 253
      Top = 0
      Width = 336
      Height = 247
      Align = alClient
      TabOrder = 1
      object laBottomAboveText: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 41
        Width = 328
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Text above the scene'
        ExplicitWidth = 105
      end
      object laBottomBelowText: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 1
        Width = 328
        Height = 13
        Margins.Top = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Text below the scene'
        ExplicitWidth = 103
      end
      object laBottomChangeGlobalTransparency: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 141
        Width = 328
        Height = 13
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Change global transparency'
        ExplicitWidth = 135
      end
      object ckBottomEnableTransparency: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 121
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Enable alpha transparency'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = ckBottomEnableTransparencyClick
      end
      object ckBottomShowAboveText: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 101
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Show above text'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = ckBottomShowAboveTextClick
      end
      object ckBottomShowBelowText: TCheckBox
        AlignWithMargins = True
        Left = 4
        Top = 81
        Width = 328
        Height = 17
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Show below text'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = ckBottomShowBelowTextClick
      end
      object edBottomAboveText: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 57
        Width = 328
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        TabOrder = 1
        Text = 'This text is written above the scene'
        OnChange = OnBottomTextChange
      end
      object edBottomBelowText: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 328
        Height = 21
        Margins.Bottom = 0
        Align = alTop
        TabOrder = 0
        Text = 'This text is written below the scene'
        OnChange = OnBottomTextChange
      end
      object paBottomChangeColor: TPanel
        Left = 1
        Top = 202
        Width = 334
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 6
        object laBottomChangeColor: TLabel
          Left = 0
          Top = 0
          Width = 94
          Height = 40
          Align = alLeft
          Caption = 'Change scene color'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object paBottomChangeColorSelect: TPanel
          AlignWithMargins = True
          Left = 299
          Top = 3
          Width = 32
          Height = 34
          Cursor = crHandPoint
          Align = alRight
          BevelOuter = bvNone
          Color = clYellow
          ParentBackground = False
          TabOrder = 1
          OnClick = paBottomChangeColorSelectClick
        end
        object paBottomChangeColorAlpha: TPanel
          Left = 168
          Top = 0
          Width = 128
          Height = 40
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 0
          object laBottomChangeColorAlpha: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 122
            Height = 13
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Alpha'
            ExplicitWidth = 27
          end
          object tbBottomChangeColorAlpha: TTrackBar
            AlignWithMargins = True
            Left = 3
            Top = 19
            Width = 122
            Height = 20
            Margins.Bottom = 0
            Align = alTop
            Max = 255
            Frequency = 25
            Position = 128
            ShowSelRange = False
            TabOrder = 0
            OnChange = tbBottomChangeColorAlphaChange
          end
        end
      end
      object tbBottomGlobalTransparency: TTrackBar
        AlignWithMargins = True
        Left = 4
        Top = 157
        Width = 328
        Height = 45
        Margins.Bottom = 0
        Align = alTop
        Max = 255
        Frequency = 25
        Position = 255
        TabOrder = 5
        OnChange = tbBottomGlobalTransparencyChange
      end
    end
  end
  object cdColors: TColorDialog
    Left = 24
    Top = 16
  end
end
