object Options: TOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 168
  ClientWidth = 207
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object laFPS: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 95
    Width = 201
    Height = 13
    Align = alTop
    Caption = 'FPS'
    ExplicitWidth = 18
  end
  object ckFullScreen: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Full screen'
    TabOrder = 0
  end
  object ckUseShader: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 26
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Use shader'
    TabOrder = 1
  end
  object ckUsePreCalculatedLighting: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Use pre-calculated lighting'
    TabOrder = 2
  end
  object ckCollisions: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 72
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Collisions'
    TabOrder = 3
  end
  object paFPS: TPanel
    Left = 0
    Top = 111
    Width = 207
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 4
    object edFPS: TEdit
      Left = 0
      Top = 0
      Width = 191
      Height = 21
      Align = alClient
      NumbersOnly = True
      TabOrder = 0
      Text = '12'
      OnExit = edFPSExit
    end
    object udFPS: TUpDown
      Left = 191
      Top = 0
      Width = 16
      Height = 21
      Associate = edFPS
      Min = 1
      Position = 12
      TabOrder = 1
    end
  end
  object btOk: TButton
    Left = 129
    Top = 138
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 5
    OnClick = btOkClick
  end
end
