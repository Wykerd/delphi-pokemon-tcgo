object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 510
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 0
    Top = 0
    Width = 635
    Height = 75
    Align = alTop
    Caption = 'Start Server'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnJoin: TButton
    Left = 0
    Top = 75
    Width = 635
    Height = 75
    Align = alTop
    Caption = 'Join Server'
    TabOrder = 1
    OnClick = btnJoinClick
  end
  object redDebug: TRichEdit
    Left = 0
    Top = 150
    Width = 635
    Height = 180
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
    OnChange = redDebugChange
  end
  object redClient: TRichEdit
    Left = 0
    Top = 330
    Width = 635
    Height = 180
    Align = alTop
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 3
    OnChange = redClientChange
  end
end
