object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Watercan - MD3 renderer'
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
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object paRendering: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 527
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    DoubleBuffered = False
    FullRepaint = False
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
  end
end
