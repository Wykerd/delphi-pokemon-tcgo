program game;

// Compile the game resources
{$R 'uires.res' 'UI\resources\uires.rc'}
{$R 'uicomponentres.res' 'UI\components\resources\uicomponentres.rc'}
{$R 'textureres.res' 'UI\textures\textureres.rc'}
{$R 'game_components_res.res' 'UI\game_components\resources\game_components_res.rc'}

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
  UIImgButton in 'UI\components\UIImgButton.pas',
  Textures in 'dependencies\Textures.pas',
  pkmCard in 'UI\game_components\pkmCard.pas',
  gameLogic in 'gameLogic.pas',
  gameState in 'gameState.pas',
  cardDeck in 'cardDeck.pas',
  clientState in 'clientState.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLauncher, frmLauncher);
  Application.CreateForm(TdmDB, dmDB);
  Application.CreateForm(TfrmUserEditor, frmUserEditor);
  Application.Run;
end.

