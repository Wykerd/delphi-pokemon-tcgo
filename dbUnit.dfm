object dmDB: TdmDB
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 336
  Width = 219
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
    Left = 120
    Top = 16
  end
  object tblCards: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Cards'
    Left = 56
    Top = 64
  end
  object tblAttacks: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Attacks'
    Left = 56
    Top = 112
  end
  object tblPokemon: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Pokemon'
    Left = 56
    Top = 160
  end
  object tblTrainers: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Trainers'
    Left = 56
    Top = 264
  end
  object tblSets: TADOTable
    Connection = con
    CursorType = ctStatic
    TableName = 'Sets'
    Left = 56
    Top = 208
  end
  object DataSource1: TDataSource
    DataSet = tblCards
    Left = 120
    Top = 64
  end
  object DataSource2: TDataSource
    DataSet = tblAttacks
    Left = 120
    Top = 112
  end
  object DataSource3: TDataSource
    DataSet = tblPokemon
    Left = 120
    Top = 160
  end
  object DataSource4: TDataSource
    DataSet = tblSets
    Left = 120
    Top = 208
  end
  object DataSource5: TDataSource
    DataSet = tblTrainers
    Left = 120
    Top = 264
  end
end
