object frmLauncher: TfrmLauncher
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Card Game'
  ClientHeight = 620
  ClientWidth = 645
  Color = clBtnFace
  DoubleBuffered = True
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
    Width = 645
    Height = 75
    Align = alTop
    Caption = 'Start Local Server'
    TabOrder = 0
    OnClick = btnStartClick
    ExplicitWidth = 569
  end
  object btnJoin: TButton
    Left = 0
    Top = 75
    Width = 645
    Height = 75
    Align = alTop
    Caption = 'Start Game Client'
    TabOrder = 1
    OnClick = btnJoinClick
  end
  object redDebug: TRichEdit
    Left = 0
    Top = 235
    Width = 645
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
    Zoom = 100
    OnChange = redDebugChange
  end
  object redClient: TRichEdit
    Left = 0
    Top = 415
    Width = 645
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
    Zoom = 100
    OnChange = redClientChange
  end
  object pnlUser: TPanel
    Left = 0
    Top = 150
    Width = 645
    Height = 85
    Align = alTop
    ParentBackground = False
    TabOrder = 4
    ExplicitLeft = -112
    ExplicitTop = 126
    object lblUser: TLabel
      Left = 16
      Top = 16
      Width = 3
      Height = 13
    end
    object lblUID: TLabel
      Left = 16
      Top = 35
      Width = 3
      Height = 13
    end
    object btnUser: TButton
      Left = 1
      Top = 59
      Width = 643
      Height = 25
      Align = alBottom
      Caption = 'Update Or Create User'
      TabOrder = 0
      OnClick = btnUserClick
    end
  end
  object btnAdmin: TButton
    Left = 0
    Top = 595
    Width = 645
    Height = 25
    Align = alClient
    Caption = 'Admin Console'
    TabOrder = 5
    OnClick = btnAdminClick
    ExplicitLeft = -1
    ExplicitTop = 240
  end
end
