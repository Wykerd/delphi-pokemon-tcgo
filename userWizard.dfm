object frmUserEditor: TfrmUserEditor
  Left = 0
  Top = 0
  Caption = 'User Creation Wizard'
  ClientHeight = 115
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object edtUsername: TLabeledEdit
    Left = 8
    Top = 24
    Width = 233
    Height = 21
    EditLabel.Width = 48
    EditLabel.Height = 13
    EditLabel.Caption = 'Username'
    TabOrder = 0
  end
  object btnUpdate: TButton
    Left = 8
    Top = 51
    Width = 233
    Height = 25
    Caption = 'Update User'
    TabOrder = 1
    OnClick = btnUpdateClick
  end
  object btnCreate: TButton
    Left = 8
    Top = 82
    Width = 233
    Height = 25
    Caption = 'Create User'
    TabOrder = 2
    OnClick = btnCreateClick
  end
end
