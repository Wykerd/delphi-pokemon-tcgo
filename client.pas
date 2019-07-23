unit client;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdContext, IdThreadComponent, ComCtrls,
  Graphics, SysUtils, helpers, DBXJSON;

type
  TClient = class (TIdTCPClient)
  private
    FDebug: TRichEdit;
    FIdThread : TIdThreadComponent;
    FAuthLock: string;
    FAuthenticated: boolean;
    FCredentials: TJSONPair;
    procedure SetDebug(const Value: TRichEdit);
    procedure Run (Sender: TIdThreadComponent);
    procedure println (t, s : string);
    procedure Connected(Sender: TObject);
    procedure Disconnected(Sender: TObject);
    procedure SetAuthLock(const Value: string);
    procedure SetAuthenticated(const Value: boolean);
    procedure SetCredentials(const Value: TJSONPair);
  published
    constructor Create (AOwner: TComponent);
    procedure Start (Host: string; Port: integer);
    property Debug : TRichEdit read FDebug write SetDebug;
    procedure Authenticate;
    property AuthLock : string read FAuthLock write SetAuthLock;
    property Authenticated : boolean read FAuthenticated write SetAuthenticated;
    property Credentials : TJSONPair read FCredentials write SetCredentials;
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

class function TClient.GetCredentials (AuthPath : string) : TJSONObject;
var
  tF : textfile;
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
    JSON := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(data),0));
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
    println('error', 'Could not locate the authentication file! One has to be created in the laucher!');
    exit;
  end;

  AssignFile(tF, AuthLock);
  try
    try
      reset(tF);
    except
      println('error', 'Error while opening the config file, is it already open?');
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
        println('error', 'Invalid authentication file (no auth object). Create new one in the launcher');
      end
      else if TJSONObject(JSON.Get('auth').JsonValue).get('uid') = nil then
      begin
        println('error', 'Invalid authentication file (no uid). Create new one in the launcher!');
      end;
      
      JSONRes.AddPair(TJSONPair.Create('action', 'authenticate'));
      JSONRes.AddPair(JSON.Get('auth'));

      // Send the authentication request to the server
      IOHandler.WriteLn(JSONRes.ToString);
      println('auth', 'Attempting to authenticate with server');
      
      // Set the credentials for future use;
      Credentials := JSON.Get('auth');
    except
      println('error', 'An eror occured while reading / sending the authentication payload');
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
  AuthLock := GetCurrentDir + '\auth.json';
  Authenticated := false;
  Credentials := nil;
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
    Showmessage('Error creating UID, Try again!');
    Halt;
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
  Debug.SelAttributes.Color := clGreen;
  Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
end;

procedure TClient.Run(Sender: TIdThreadComponent);
begin
  println('incoming', IOHandler.ReadLn());
end;

procedure TClient.SetAuthenticated(const Value: boolean);
begin
  FAuthenticated := Value;
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

procedure TClient.Start(Host: string; Port: integer);
begin
  Self.Host := Host;
  Self.Port := Port;
  Connect;
  Authenticate;
end;

end.
