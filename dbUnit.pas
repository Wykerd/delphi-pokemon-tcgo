unit dbUnit;

interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TdmDB = class(TDataModule)
    con: TADOConnection;
    tblUsers: TADOTable;
    dsc: TDataSource;
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
  con.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='
    + GetCurrentDir + '\server\database.mdb;Mode=ReadWrite;Persist Security Info=False';
  con.LoginPrompt := False;
  con.Open;
  tblUsers.Connection := con;
  tblUsers.TableName := 'Users';
  tblUsers.Active := true;
end;

end.
