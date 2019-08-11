program game;

uses
  Forms,
  launcher in 'launcher.pas' {frmLauncher},
  server in 'server.pas',
  client in 'client.pas',
  dbUnit in 'dbUnit.pas' {dmDB: TDataModule},
  helpers in 'helpers.pas',
  userWizard in 'userWizard.pas' {frmUserEditor},
  serverConfig in 'serverConfig.pas',
  startUI in 'UI\startUI.pas',
  clientUI in 'UI\clientUI.pas',
  gameUI in 'UI\gameUI.pas',
  serversUI in 'UI\serversUI.pas',
  UIButton in 'UI\components\UIButton.pas',
  UIContainer in 'UI\components\UIContainer.pas',
  tradeUI in 'UI\tradeUI.pas',
  profileUI in 'UI\profileUI.pas',
  serverSessions in 'serverSessions.pas',
  UIImgButton in 'UI\components\UIImgButton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLauncher, frmLauncher);
  Application.CreateForm(TdmDB, dmDB);
  Application.CreateForm(TfrmUserEditor, frmUserEditor);
  Application.Run;
end.

