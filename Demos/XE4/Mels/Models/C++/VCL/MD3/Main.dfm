object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Cabinet with glass pane - MD3 renderer'
  ClientHeight = 537
  ClientWidth = 599
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object paRendering: TPanel
    Left = 0
    Top = 0
    Width = 599
    Height = 537
    Cursor = crCross
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    DoubleBuffered = False
    FullRepaint = False
    ParentBackground = False
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitWidth = 589
    ExplicitHeight = 527
  end
end
