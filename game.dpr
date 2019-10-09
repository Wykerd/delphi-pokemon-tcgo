program game;

// Compile the game resources
{$R 'uires.res' 'UI\resources\uires.rc'}
{$R 'uicomponentres.res' 'UI\components\resources\uicomponentres.rc'}
{$R 'game_components_res.res' 'UI\game_components\resources\game_components_res.rc'}
{$R 'textureres.res' 'UI\textures\textureres.rc'}

uses
  Forms,
  Windows,
  uCEFApplication,
  uCEFTypes,
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
  serverSessions in 'serverSessions.pas',
  UIImgButton in 'UI\components\UIImgButton.pas',
  Textures in 'dependencies\Textures.pas',
  pkmCard in 'UI\game_components\pkmCard.pas',
  gameLogic in 'gameLogic.pas',
  gameState in 'gameState.pas',
  cardDeck in 'cardDeck.pas',
  clientState in 'clientState.pas',
  preGameUI in 'UI\preGameUI.pas',
  cardData in 'cardData.pas',
  versions in 'versions.pas',
  gameActionUI in 'UI\gameActionUI.pas',
  adminConsole in 'adminConsole.pas' {frmAdminConsole};

{$R *.res}

begin
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.EnableGPU := True;      // Enable hardware acceleration
  GlobalCEFApp.UserDataPath := 'cef_userdata';
  GlobalCEFApp.Cache := 'cef_cache';
  GlobalCEFApp.PersistSessionCookies := True;
  GlobalCEFApp.DisableFeatures  := 'NetworkService,OutOfBlinkCors';

  if GlobalCEFApp.StartMainProcess then
    begin
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfrmLauncher, frmLauncher);
  Application.CreateForm(TdmDB, dmDB);
  Application.CreateForm(TfrmUserEditor, frmUserEditor);
  Application.CreateForm(TfrmAdminConsole, frmAdminConsole);
  Application.Run;
    end;

  DestroyGlobalCEFApp;
end.

