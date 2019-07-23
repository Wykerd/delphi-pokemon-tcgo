object dmDB: TdmDB
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 73
  Width = 143
  object con: TADOConnection
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 16
    Top = 16
  end
  object tblUsers: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Users'
    Left = 56
    Top = 16
  end
  object dsc: TDataSource
    DataSet = tblUsers
    Left = 96
    Top = 16
  end
end
