unit userWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DBXJSON, client, helpers;

type
  TfrmUserEditor = class(TForm)
    edtUsername: TLabeledEdit;
    btnUpdate: TButton;
    btnCreate: TButton;
    procedure btnUpdateClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
  private
    { Private declarations }
    Username, UID : string;
    call : TObjProcedure;
  public
    { Public declarations }
    procedure LaunchWizard(sUsername, sUID: string; callback: TObjProcedure); overload;
    procedure LaunchWizard(callback: TObjProcedure); overload;
  end;

var
  frmUserEditor: TfrmUserEditor;

implementation

{$R *.dfm}

procedure TfrmUserEditor.LaunchWizard(sUsername, sUID: string; callback: TObjProcedure);
begin
  btnUpdate.Enabled := true;
  Username := sUsername;
  edtUsername.Text := Username;
  UID := sUID;
  call := callback;
  Show;
end;

procedure TfrmUserEditor.btnCreateClick(Sender: TObject);
begin
  TClient.CreateCredentials(edtUsername.Text, GetCurrentDir + '\client\auth.json');
  call;
  hide;
end;

procedure TfrmUserEditor.btnUpdateClick(Sender: TObject);
begin
  TClient.CreateCredentials(edtUsername.Text, UID, GetCurrentDir + '\client\auth.json');
  call;
  hide;
end;

procedure TfrmUserEditor.LaunchWizard(callback: TObjProcedure);
begin
  btnUpdate.Enabled := false;
  call := callback;
  show;
end;

end.
