program game;

uses
  Forms,
  launcher in 'launcher.pas' {frmLauncher},
  server in 'server.pas',
  client in 'client.pas',
  dbUnit in 'dbUnit.pas' {dmDB: TDataModule},
  helpers in 'helpers.pas',
  userWizard in 'userWizard.pas' {frmUserEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLauncher, frmLauncher);
  Application.CreateForm(TdmDB, dmDB);
  Application.CreateForm(TfrmUserEditor, frmUserEditor);
  Application.Run;
end.
