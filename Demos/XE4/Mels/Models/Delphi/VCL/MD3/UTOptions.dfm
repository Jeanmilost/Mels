object Options: TOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 103
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
  object ckFullScreen: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Full screen'
    Enabled = False
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
  object ckCollisions: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 201
    Height = 17
    Align = alTop
    Caption = 'Collisions'
    TabOrder = 2
  end
  object btOk: TButton
    Left = 129
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 3
    OnClick = btOkClick
  end
end
