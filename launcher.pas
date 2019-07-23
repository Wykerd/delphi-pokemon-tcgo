unit launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, server, ComCtrls, client, DBXJSON, helpers;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    btnJoin: TButton;
    redDebug: TRichEdit;
    redClient: TRichEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure redDebugChange(Sender: TObject);
    procedure redClientChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    jsCredentials : TJSONObject;
    bLaunchable : boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Server: TServer;
  Client: TClient;

implementation

{$R *.dfm}

procedure TForm1.btnJoinClick(Sender: TObject);
begin
  if not bLaunchable then
  begin
    showmessage('You have to create an account before you can join an server!');
    exit;
  end;

  Client := TClient.Create(Self);
  Client.Debug := redClient;

  // Start the connection to the client
  Client.Start(InputBox('Host', 'Enter the IP for server', 'localhost'), 8080);
  // Client.CreateCredentials('Wykerd');
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  TButton(Sender).Enabled := false;
  Server := TServer.Create(Self);
  Server.Debug := redDebug;
  Server.Start(8080);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Client <> nil then
  begin
    Client.IOHandler.InputBuffer.clear;
    Client.Disconnect;
    Client.Destroy;
  end;

  if Server <> nil then Server.Destroy;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  auth : TJSONObject;
begin
  bLaunchable := false;

  // Get Credentials
  jsCredentials := TClient.GetCredentials(GetCurrentDir + '\auth.json') as TJSONObject;

  // Validate credentials object
  if jsCredentials <> nil then
    showmessage(jsCredentials.ToString);

  // Pass
  bLaunchable := true;
end;

procedure TForm1.redClientChange(Sender: TObject);
begin
  // Log the debug to a file in event of crashed
  redClient.Lines.SaveToFile('client_log.rtf');
end;

procedure TForm1.redDebugChange(Sender: TObject);
begin
  // Log the debug to a file in event of crashed
  redDebug.Lines.SaveToFile('server_log.rtf');
end;

end.
