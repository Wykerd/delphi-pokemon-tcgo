object frmAdminConsole: TfrmAdminConsole
  Left = 0
  Top = 0
  Caption = 'Admin Console'
  ClientHeight = 609
  ClientWidth = 988
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid: TDBGrid
    Left = 0
    Top = 0
    Width = 988
    Height = 568
    Align = alClient
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Panel1: TPanel
    Left = 0
    Top = 568
    Width = 988
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnSort: TButton
      Left = 16
      Top = 6
      Width = 137
      Height = 25
      Caption = 'Sort Last Seen Asending'
      TabOrder = 0
      OnClick = btnSortClick
    end
    object btnAverage: TButton
      Left = 159
      Top = 6
      Width = 146
      Height = 25
      Caption = 'Calculate Average Wins'
      TabOrder = 1
      OnClick = btnAverageClick
    end
    object btnHighest: TButton
      Left = 311
      Top = 6
      Width = 202
      Height = 25
      Caption = 'Get user with highest amount of wins'
      TabOrder = 2
      OnClick = btnHighestClick
    end
    object btnDelete: TButton
      Left = 519
      Top = 6
      Width = 114
      Height = 25
      Caption = 'Delete user by ID'
      TabOrder = 3
      OnClick = btnDeleteClick
    end
  end
end
