unit adminConsole;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Data.DB, Vcl.Grids,
  Vcl.DBGrids, dbUnit, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmAdminConsole = class(TForm)
    DBGrid: TDBGrid;
    Panel1: TPanel;
    btnSort: TButton;
    btnAverage: TButton;
    btnHighest: TButton;
    btnDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnAverageClick(Sender: TObject);
    procedure btnHighestClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAdminConsole: TfrmAdminConsole;

implementation

{$R *.dfm}

procedure TfrmAdminConsole.btnAverageClick(Sender: TObject);
var
  total : integer;
begin
  total := 0;

  with dmDB do
  begin
    tblUsers.Open;
    tblUsers.First;
    while not tblUsers.eof do
    begin
      total := total + tblUsers['Wins'];
      tblUsers.Next;
    end;
    showmessagefmt('The average user has %.2f wins', [total / tblUsers.RecordCount]);
  end;
end;

procedure TfrmAdminConsole.btnDeleteClick(Sender: TObject);
begin
  dmDB.tblUsers.Open;
  if dmDB.tblUsers.Locate('ID', strtoint(inputbox('Delete User', 'Enter the ID of the user to delete', '')), []) then
  begin
    if MessageDlg(format('Are you sure you want to delete user %s, with username %s ?',
      [dmDB.tblUsers['UID'], dmDB.tblUsers['UserName']]), mtWarning, [mbYes, mbNo], 0) = mrYes then
    begin
      dmDB.tblUsers.Delete;
    end;
  end;
end;

procedure TfrmAdminConsole.btnHighestClick(Sender: TObject);
var
  id, iWins: integer;
  sName: string;
begin
  with dmDB do
  begin
    tblUsers.Open;
    tblUsers.First;
    iWins := tblUsers['Wins'];
    sName := tblUsers['UserName'];
    id := tblUsers['ID'];
    while not tblUsers.eof do
    begin
      if iWins < tblUsers['Wins'] then
      begin
        iWins := tblUsers['Wins'];
        sName := tblUsers['UserName'];
        id := tblUsers['ID'];
      end;
      tblUsers.Next;
    end;
    showmessagefmt('%s with ID %d has the most wins, with %d wins.', [sName, id, iWins]);
  end;
end;

procedure TfrmAdminConsole.btnSortClick(Sender: TObject);
begin
  dmDB.tblUsers.Sort := 'LastLogin ASC';
end;

procedure TfrmAdminConsole.FormShow(Sender: TObject);
begin
  DBGrid.DataSource := dmDB.dsc;
end;

end.
