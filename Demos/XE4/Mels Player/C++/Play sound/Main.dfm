object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Play sound'
  ClientHeight = 136
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object paFile: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 282
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object edFileName: TEdit
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 241
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alClient
      Enabled = False
      TabOrder = 0
    end
    object btSelect: TButton
      Left = 246
      Top = 0
      Width = 36
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btSelectClick
    end
  end
  object btPlay: TButton
    AlignWithMargins = True
    Left = 5
    Top = 36
    Width = 282
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Play'
    TabOrder = 1
    OnClick = btPlayClick
  end
  object btPause: TButton
    AlignWithMargins = True
    Left = 5
    Top = 71
    Width = 282
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Pause'
    Enabled = False
    TabOrder = 2
    OnClick = btPauseClick
  end
  object btStop: TButton
    AlignWithMargins = True
    Left = 5
    Top = 106
    Width = 282
    Height = 25
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = 'Stop'
    Enabled = False
    TabOrder = 3
    OnClick = btStopClick
  end
  object plPlayer: TQRPlayerAL
    Left = 8
    Top = 8
  end
  object odOpen: TOpenDialog
    Left = 42
    Top = 8
  end
  object tiTimer: TTimer
    OnTimer = tiTimerTimer
    Left = 77
    Top = 8
  end
end
