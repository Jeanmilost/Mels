object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Threaded jobs'
  ClientHeight = 400
  ClientWidth = 400
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
  object paButtons: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 372
    Width = 394
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btAdd: TButton
      Left = 319
      Top = 0
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Add'
      TabOrder = 0
      OnClick = btAddClick
    end
    object btDelete: TButton
      Left = 244
      Top = 0
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Delete'
      TabOrder = 1
      OnClick = btDeleteClick
    end
  end
  object lbData: TListBox
    Left = 0
    Top = 0
    Width = 400
    Height = 369
    Align = alClient
    ItemHeight = 13
    TabOrder = 1
  end
end
