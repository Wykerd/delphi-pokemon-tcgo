unit client;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdContext, IdThreadComponent, ComCtrls,
  Graphics, SysUtils, helpers, DBXJSON, clientUI, clientState;

type
  TClient = class (TIdTCPClient)
  private
    FDebug: TRichEdit;
    FIdThread : TIdThreadComponent;
    FAuthLock: string;
    FAuthenticated: boolean;
    FCredentials: TJSONPair;
    FUI: TClientUI;
    FServersLock: string;
    FServerIndex: integer;
    FGameState: TClientState;
    procedure SetDebug(const Value: TRichEdit);
    procedure Run (Sender: TIdThreadComponent);
    // for main thread prints
    procedure println (t, s : string);
    // for worker thread prints
    procedure threadprint(t, s: string);
    procedure Connected(Sender: TObject);
    procedure Disconnected(Sender: TObject);
    procedure SetAuthLock(const Value: string);
    procedure SetAuthenticated(const Value: boolean);
    procedure SetCredentials(const Value: TJSONPair);
    procedure SetUI(const Value: TClientUI);
    procedure SetServersLock(const Value: string);
    procedure SetServerIndex(const Value: integer);
    procedure SetGameState(const Value: TClientState);
  published
    constructor Create (AOwner: TComponent);
    procedure Start (Host: string; Port: integer; ServerIndex: integer = -1);
    property Debug : TRichEdit read FDebug write SetDebug;
    procedure Authenticate;
    property AuthLock : string read FAuthLock write SetAuthLock;
    property ServersLock : string read FServersLock write SetServersLock;
    property Authenticated : boolean read FAuthenticated write SetAuthenticated;
    property Credentials : TJSONPair read FCredentials write SetCredentials;
    property UI : TClientUI read FUI write SetUI;
    property ServerIndex: integer read FServerIndex write SetServerIndex;
    procedure SendChat (s: string);
    // Actions
    procedure UpdateServerListings (Index: integer; Motd: string; Name: string; CustomName: string = '');
    procedure ExecuteAction (s : string);
    // State
    property GameState : TClientState read FGameState write SetGameState;
    procedure PushStateToUI;
  public
    class function GetCredentials (AuthPath : string)  : TJSONObject;
    class procedure CreateCredentials(sUsername, AuthLock : string); overload;
    class procedure CreateCredentials(sUsername, sUID, AuthLock : string); overload;
  end;

implementation

{ TClient }

procedure TClient.Connected(Sender: TObject);
begin
  FIdThread.Active := true;
  println('status', 'Connected');
end;

procedure TClient.Disconnected(Sender: TObject);
begin
  println('status', 'Disconnected');
end;

procedure TClient.ExecuteAction(s: string);
var
  action : string;
  data, JSON: TJSONObject;
begin
  JSON := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(s),0));
  if JSON <> nil then
  begin
    // Check if payload has action
    if JSON.Get('action') = nil then
    begin
      threadprint('action', 'ERROR: no action in payload!');
      exit;
    end;
    // Check for data
    if JSON.Get('data') = nil then
    begin
      threadprint('action', 'ERROR: no data property in payload');
      exit;
    end;

    data := TJSONObject(JSON.Get('data').JsonValue);

    if data = nil then
    begin
      threadprint('action', 'ERROR: empty data property in payload');
      exit;
    end;
    //
    action := JSON.get('action').JsonValue.Value;

    // Run the actions //
    if action = 'authenticate' then
    begin
      if data.Get('success') <> nil then
      begin
        if data.Get('success').JsonValue.Value = 'true' then
        begin
          if data.Get('sid') <> nil then
          begin
            threadprint('auth', 'Successfully authenticated');
            // Add the session to the auth credentials!
            TJSONObject(Credentials.JsonValue).AddPair(data.Get('sid'));
            // request the latest server info to update the cache
            IOHandler.WriteLn('{"action":"listing-info","auth":' + TJSONObject(Credentials.JsonValue).ToString + '}');
            Authenticated := true;
          end
          else
            threadprint('auth', 'Authentication payload does not contain the field "sid"');
        end
        else
          threadprint('auth', 'Could not authenticate');
      end;
    end; // end authenticate

    if (action = 'join') OR (action = 'chat') then
    begin
      if data.Get('message') <> nil then
      begin
        tthread.Synchronize(procedure begin UI.GameUI.IncomingChat(data.Get('message').JsonValue.Value); end);
      end;
    end;

    // Update local server listing cache
    if action = 'listing-info' then
    begin
      if not ((data.Get('motd') = nil) or (data.Get('name') = nil)) then
      begin
        if ServerIndex > -1 then
          UpdateServerListings(ServerIndex, data.Get('motd').JsonValue.Value, data.Get('name').JsonValue.Value);
      end;
    end;

    // End actions //
  end
  else
  begin
    threadprint('action', 'ERROR: Invalid JSON payload');
  end;
end;

class function TClient.GetCredentials (AuthPath : string) : TJSONObject;
var
  tF: textfile;
  data, tmp : string;
  JSON : TJSONObject;

begin
  // Init
  data := '';
  tmp := '';

  // Check if the auth file is available
  if not fileExists(AuthPath) then
  begin
    result := nil;
    exit;
  end;

  AssignFile(tF, AuthPath);
  try
    try
      reset(tF);
    except
      result := nil;
    end;
  finally
    while not eof(tF) do
    begin
      readln(tF, tmp);
      data := data + tmp;
    end;


    closefile(tf);

    // Parse the JSON
    JSON := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(data)),0));
    result := JSON;
  end;

end;

procedure TClient.Authenticate;
var
  tF : textfile;
  data, tmp : string;
  JSON, JSONRes : TJSONObject;

begin
  // Init
  data := '';
  tmp := '';

  // Check if the auth file is available
  if not fileExists(AuthLock) then
  begin
    threadprint('error', 'Could not locate the authentication file! One has to be created in the laucher!');
    exit;
  end;

  AssignFile(tF, AuthLock);
  try
    try
      reset(tF);
    except
      threadprint('error', 'Error while opening the config file, is it already open?');
    end;
  finally
    while not eof(tF) do
    begin
      readln(tF, tmp);
      data := data + tmp;
    end;
    closefile(tf);

    // Parse the JSON
    JSONRes := TJSONObject.Create;
    try
      JSON := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(data),0));
      if JSON.Get('auth') = nil then
      begin
        threadprint('error', 'Invalid authentication file (no auth object). Create new one in the launcher');
      end
      else if TJSONObject(JSON.Get('auth').JsonValue).get('uid') = nil then
      begin
        threadprint('error', 'Invalid authentication file (no uid). Create new one in the launcher!');
      end;

      JSONRes.AddPair(TJSONPair.Create('action', 'authenticate'));
      JSONRes.AddPair(JSON.Get('auth'));

      // Send the authentication request to the server
      IOHandler.WriteLn(JSONRes.ToString);
      threadprint('auth', 'Attempting to authenticate with server');

      // Set the credentials for future use;
      Credentials := JSON.Get('auth');
    except
      threadprint('error', 'An eror occured while reading / sending the authentication payload');
    end;
  end;

end;

constructor TClient.Create(AOwner: TComponent);
begin
  inherited;
  FIdThread := TIdThreadComponent.Create(AOwner);
  FIdThread.OnRun := Run;
  OnDisconnected := Disconnected;
  OnConnected := Connected;
  AuthLock := GetCurrentDir + '\client\auth.json';
  ServersLock := GetCurrentDir + '\client\server-list.json';
  ServerIndex := -1;
  Credentials := nil;
  UI := TClientUI.CreateNew(Self, 0);
  UI.StartTrigger := Start;
  UI.GameUI.OnChat := SendChat;
  Authenticated := false;
end;

// Use an pre-existing UID
class procedure TClient.CreateCredentials(sUsername, sUID, AuthLock : string);
var
  tF: TextFile;
  JSONRoot, JSONAuth : TJSONObject;
begin
  JSONRoot := TJSONObject.Create;
  JSONAuth := TJSONObject.Create;
  try
    // Build the JSON object
    JSONAuth.AddPair(TJSONPair.Create('username', sUsername));
    JSONAuth.AddPair(TJSONPair.Create('uid', sUID));
    JSONRoot.AddPair(TJSONPair.Create('auth', JSONAuth));

    // Save the authentication as a json file
    assignfile(tF, AuthLock);
    rewrite(tF);
    Writeln(tf, JSONRoot.ToString);
    closefile(tf);
  finally
    JSONRoot.Free;
  end;
end;

class procedure TClient.CreateCredentials(sUsername, AuthLock: string);
var
  tF: TextFile;
  _sUID : string;
  Uid: TGuid;
  res: HResult;
  JSONRoot, JSONAuth : TJSONObject;
begin
  // Create an unique user id that can be used on any server regardless of the username.
  // As we cannot assure that usernames are unique without a sentral server.
  // This ensures that each user has the same unique ID across multiple servers
  res := CreateGUID(Uid);
  if res = S_OK then
    _sUID := GuidToString(Uid)
  else
  begin
    // If an id could not be generated terminate the program
    // Happens so rarely should not cause any problems
    raise Exception.Create('Client Credential Creation Error: Could not generate UID');
    exit;
  end;

  JSONRoot := TJSONObject.Create;
  JSONAuth := TJSONObject.Create;
  try
    // Build the JSON object
    JSONAuth.AddPair(TJSONPair.Create('username', sUsername));
    JSONAuth.AddPair(TJSONPair.Create('uid', _sUID));
    JSONRoot.AddPair(TJSONPair.Create('auth', JSONAuth));

    // Save the authentication as a json file
    assignfile(tF, AuthLock);
    rewrite(tF);
    Writeln(tf, JSONRoot.ToString);
    closefile(tf);
  finally
    JSONRoot.Free;
  end;
end;

procedure TClient.println(t, s: string);
begin
  Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
end;

procedure TClient.PushStateToUI;
begin
  tthread.Synchronize(procedure begin UI.GameUI.State := GameState; end);
end;

procedure TClient.Run(Sender: TIdThreadComponent);
var
  req : string;
  JSON : TJSONObject;
begin
  req := IOHandler.ReadLn();

  // Synchronize with main thread
  threadprint('incoming', req);
  ExecuteAction(req);
end;

procedure TClient.SendChat(s: string);
var
  JSONReq, JSONData: TJSONObject;
begin
  JSONReq := TJSONObject.Create;

  JSONReq.AddPair(TJSONPair.Create('action', 'chat'));

  // Authenticate the payload
  JSONReq.AddPair(Credentials);

  // Build the payload data
  JSONData := TJSONObject.Create;
  JSONData.AddPair(TJSONPair.create('message', s));

  JSONReq.AddPair(TJSONPair.create('data', JSONData));

  IOHandler.WriteLn(JSONReq.ToString);
end;

procedure TClient.SetAuthenticated(const Value: boolean);
begin
  FAuthenticated := Value;
  Tthread.Synchronize(procedure begin UI.PreGameUI.Authenticated := FAuthenticated; end);
end;

procedure TClient.SetAuthLock(const Value: string);
begin
  FAuthLock := Value;
end;

procedure TClient.SetCredentials(const Value: TJSONPair);
begin
  FCredentials := Value;
end;

procedure TClient.SetDebug(const Value: TRichEdit);
begin
  FDebug := Value;
end;

procedure TClient.SetGameState(const Value: TClientState);
begin
  FGameState := Value;
end;

procedure TClient.SetServerIndex(const Value: integer);
begin
  FServerIndex := Value;
end;

procedure TClient.SetServersLock(const Value: string);
begin
  FServersLock := Value;
end;

procedure TClient.SetUI(const Value: TClientUI);
begin
  FUI := Value;
end;

procedure TClient.Start(Host: string; Port: integer; ServerIndex: integer = -1);
begin
  Self.Host := Host;
  Self.Port := Port;
  Self.ServerIndex := ServerIndex;
  Connect;
  Authenticate;
end;

procedure TClient.threadprint(t, s: string);
begin
  // Queue the action to be preformed in the main thread as it is not
  // thread safe.
  TThread.Synchronize(nil,
    procedure
    begin
      Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
    end);
end;

// TODO : Add way to fix invalid json (adding properties if they dont exist rather than exiting method)
procedure TClient.UpdateServerListings(Index: integer; Motd, Name,
  CustomName: string);
var
  tF : textfile;
  JSON, JSONListing, JSONServerListing : TJSONObject;
  ARR : TJSONArray;
  tmp, data : string;
begin
  AssignFile(tf, ServersLock);

  try

    reset(tf);
  finally
    while not eof(tf) do
    begin
      Readln(tf, tmp);
      data := data + tmp;
    end;

    JSON := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(data)), 0));

    if JSON <> nil then
    begin
      if JSON.Get('servers') <> nil then
      begin
        ARR := TJSONArray(JSON.Get('servers').JsonValue);
        JSONListing := TJSONObject(ARR.Get(Index));

        if JSONListing.Get('name') <> nil then
        begin

          if JSONListing.Get('listing') <> nil then
          begin
            JSONServerListing := TJSONObject(JSONListing.Get('listing').JsonValue);

            if not ((JSONServerListing.Get('name') = nil) or (JSONServerListing.Get('motd') = nil)) then
            begin
              JSONServerListing.Get('name').JsonValue := TJSONString.Create(Name);
              JSONServerListing.Get('motd').JsonValue := TJSONString.Create(Motd);

              // if everything succeeds the file should be updated

              rewrite(tf);
              Writeln(tf, JSON.ToString);
              println('file', 'Updated ' + ServersLock);

            end;

          end;

        end;

      end;

    end;

  end;

  closefile(tF);

  //JSON.Free;
end;

end.
