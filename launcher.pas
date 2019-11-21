unit launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, server, ComCtrls, client, System.JSON, helpers, ExtCtrls,
  userWizard, pkmCard, adminConsole;

type
  TfrmLauncher = class(TForm)
    btnStart: TButton;
    btnJoin: TButton;
    redDebug: TRichEdit;
    redClient: TRichEdit;
    pnlUser: TPanel;
    btnUser: TButton;
    lblUser: TLabel;
    lblUID: TLabel;
    btnAdmin: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure redDebugChange(Sender: TObject);
    procedure redClientChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnUserClick(Sender: TObject);
    procedure btnAdminClick(Sender: TObject);
  private
    { Private declarations }
    jsCredentials : TJSONObject;
    bLaunchable : boolean;
    FUID: string;
    FUsername: string;
    procedure SetUID(const Value: string);
    procedure SetUsername(const Value: string);
    procedure SetLaunchable(const Value : boolean);
  public
    { Public declarations }
    procedure LoadCredentials;
  published
    property Username : string read FUsername write SetUsername;
    property UID : string read FUID write SetUID;
    property Launchable : boolean read bLaunchable write SetLaunchable;
  end;

var
  frmLauncher: TfrmLauncher;
  Server: TServer;
  Client: TClient;

implementation

{$R *.dfm}

procedure TfrmLauncher.btnAdminClick(Sender: TObject);
begin
  frmAdminConsole.show;
end;

procedure TfrmLauncher.btnJoinClick(Sender: TObject);
begin
  if not bLaunchable then
  begin
    showmessage('You have to create an account before you can join an server!');
    exit;
  end;
  Client := TClient.Create(Self);
  Client.UI.TradeUI.UserID := UID;
  Client.Debug := redClient;
  Client.ServersLock := 'client\server-list.json';
  Client.UI.ServersUI.LoadFromFile(Client.ServersLock);
  Client.UI.Show;
end;

procedure TfrmLauncher.btnStartClick(Sender: TObject);
begin
  TButton(Sender).Enabled := false;
  Server := TServer.Create(Self);
  Server.Debug := redDebug;
  Server.Config.LoadFromFile('server\config.json');
  Server.Start;
end;

procedure TfrmLauncher.btnUserClick(Sender: TObject);
begin
  if Launchable then
    frmUserEditor.LaunchWizard(Username, UID, loadcredentials)
  else
    frmUserEditor.LaunchWizard(loadCredentials);
end;

procedure TfrmLauncher.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Server <> nil then
  begin
    // Fixes issue where program freezes because client disconnects
    // as the server gets destroyed blocking the main and server thread;
    Server.OnDisconnect := nil;
    Server.OnExecute := nil;
    Server.Active := false;
    Server.Destroy;
  end;

  if Client <> nil then
    Client.Destroy;
end;

procedure TfrmLauncher.FormCreate(Sender: TObject);
var
  ResStream : TResourceStream;
  FontsCount : DWORD;
begin
  RELOADPKMCARDVAR;

  ResStream := TResourceStream.Create(hInstance, 'UIFont', RT_RCDATA);
  try
    AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount);
  finally
    ResStream.Free;
  end;

  LoadCredentials;

  // For Debugging - auto launch client
  btnJoin.OnClick(nil);
end;

procedure TfrmLauncher.LoadCredentials;
var
  auth : TJSONObject;
begin
  bLaunchable := false;

  // Get Credentials
  jsCredentials := TClient.GetCredentials(GetCurrentDir + '\client\auth.json') as TJSONObject;

  // Validate credentials object
  if jsCredentials = nil then
    exit;
  if jsCredentials.Get('auth') = nil then
    exit;

  auth := TJSONObject(jsCredentials.Get('auth').JsonValue);

  if auth.Get('uid') = nil then
    exit;
  if auth.Get('username') = nil then
    exit;

  Username := auth.Get('username').JsonValue.Value;
  UID := auth.Get('uid').JsonValue.Value;

  // Pass
  bLaunchable := true;
end;

procedure TfrmLauncher.redClientChange(Sender: TObject);
begin
  // Log the debug to a file in event of crashed
  redClient.Lines.SaveToFile('client\client_log.rtf');
end;

procedure TfrmLauncher.redDebugChange(Sender: TObject);
begin
  // Log the debug to a file in event of crashed
  redDebug.Lines.SaveToFile('server\server_log.rtf');
end;

procedure TfrmLauncher.SetLaunchable(const Value: boolean);
begin
  bLaunchable := Value;
  if Value then
    pnlUser.Caption := ''
  else
    pnlUser.Caption := 'Click the button to create an account!';

end;

procedure TfrmLauncher.SetUID(const Value: string);
begin
  FUID := Value;
  lblUID.Caption := 'User ID:' + #9 + Value;
end;

procedure TfrmLauncher.SetUsername(const Value: string);
begin
  FUsername := Value;
  lblUser.Caption := 'Username:' + #9 + Value;
end;

end.
