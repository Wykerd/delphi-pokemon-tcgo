unit dbUnit;

interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TdmDB = class(TDataModule)
    con: TADOConnection;
    tblUsers: TADOTable;
    dsc: TDataSource;
    tblCards: TADOTable;
    tblAttacks: TADOTable;
    tblPokemon: TADOTable;
    tblTrainers: TADOTable;
    tblSets: TADOTable;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DataSource3: TDataSource;
    DataSource4: TDataSource;
    DataSource5: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmDB: TdmDB;

implementation

{$R *.dfm}

procedure TdmDB.DataModuleCreate(Sender: TObject);
begin
  con.Close;
  con.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' +
  GetCurrentDir + '\server\database.mdb;Mode=ReadWrite;Persist Security Info=False';
  con.LoginPrompt := False;
  con.Open;
  //
  tblUsers.Connection := con;
  tblUsers.TableName := 'Users';
  tblUsers.Active := true;
  //
  tblTrainers.Connection := con;
  tblTrainers.TableName := 'Trainers';
  tblTrainers.Active := true;
  //
  tblSets.Connection := con;
  tblSets.TableName := 'Sets';
  //
  tblPokemon.Connection := con;
  tblPokemon.TableName := 'Pokemon';
  tblPokemon.Active := true;
  //
  tblCards.Connection := con;
  tblCards.TableName := 'Cards';
  tblCards.Active := true;
  //
  tblAttacks.Connection := con;
  tblAttacks.TableName := 'Attacks';
  tblAttacks.Active := true;

  dsc.DataSet := tblUsers;
  dsc.Enabled := true;
  tblUsers.Active := true;
end;

end.
