program game;

uses
  Forms,
  launcher in 'launcher.pas' {Form1},
  server in 'server.pas',
  client in 'client.pas',
  dbUnit in 'dbUnit.pas' {dmDB: TDataModule},
  helpers in 'helpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TdmDB, dmDB);
  Application.Run;
end.
