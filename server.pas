unit server;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCustomTCPServer, IdTCPServer, IdContext,
  ComCtrls, Graphics, SysUtils, IdStack, dbUnit, db, helpers, DBXJSON,
  IdIOHandlerSocket, StrUtils, IdSync;

type
  TServer = class(TIdTCPServer)
  private
    FDebug: TRichEdit;
    FSessions : array of string;
    procedure Execute(AContext: TIdContext);
    procedure Connected(AContext: TIdContext);
    procedure Disconnected(AContext: TIdContext);
    procedure Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure SetDebug(const Value: TRichEdit);
    procedure println(t, s: string);
  published
    constructor Create(AOwner: TComponent);
    procedure Start(Port: integer);
    procedure Broadcast(s: string);
    procedure RunCommand(CMD: TJSONObject);
    procedure ProcessAction(action: string; data: TJSONObject; username, uid : string);
    procedure ClientLogin(username, uid : string; callback : TIdIOHandlerSocket);
    property Debug: TRichEdit read FDebug write SetDebug;
  end;

implementation

{ TServer }

procedure TServer.Broadcast(s: string);
var
  tmpList: TList;
  contexClient: TIdContext;
  i: integer;
begin
  tmpList := Contexts.LockList;

  try
    for I := 0 to tmpList.Count - 1 do
    begin
      contexClient := tmpList[i];
      contexClient.Connection.IOHandler.WriteLn(s);
    end;
  finally
    Contexts.UnlockList;
  end;
end;

procedure TServer.ClientLogin(username, uid: string; callback : TIdIOHandlerSocket);
var
  // Dummy varable
  i : byte;
  procedure Authenticated;
  begin
    setlength(FSessions, length(FSessions) + 1);
    FSessions[High(FSessions)] := uid;
    callback.WriteLn('{"action":"authenticate","data":{"success":"true"}}');
  end;
begin
  if MatchStr(uid, FSessions) then
  begin
    println('login', 'Another user with that ID is already authenticated!');
    callback.WriteLn('{"action":"authenticate","data":{"success":"false"}}');
    exit;
  end;
  if dmDB.tblUsers.Locate('UID', uid, []) then
  begin
    // user found
    println('login', 'User found and logged in');
    Authenticated;
    // check that username is up to date on server side
    with dmDB do
    begin
      tblUsers.Filter := 'UID=' + QuotedStr(uid);
      tblUsers.Filtered := true;
      tblUsers.First;
      if tblUsers['UserName'] <> username then
      begin
        tblUsers.Edit;
        tblUsers['UserName'] := username;
        tblUsers.Post;
        println('login', 'Username did not match server and is now updated');
        Authenticated;
      end;
    end;
  end
  else
  begin
    with dmDB do
    begin
      tblUsers.Last;
      tblUsers.Insert;
      tblUsers['UserName'] := username;
      tblUsers['UID'] := uid;
      tblUsers.Post;
    end;
    println('login', 'User created!');
    Authenticated;

  end;
  BroadCast('{"action":"join","data":{"message":"' + username + ' has joined the game"}}');
end;

procedure TServer.Connected(AContext: TIdContext);
begin
  println('client', AContext.Binding.PeerIP + ' - Connected');
end;

constructor TServer.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FSessions, 0);
  OnExecute := Execute;
  OnConnect := Connected;
  OnStatus := Status;
  OnDisconnect := Disconnected;
  Debug := TRichEdit.Create(nil);
end;

procedure TServer.Disconnected(AContext: TIdContext);
begin
  println('client', AContext.Binding.PeerIP + ' - Disconnected');
end;

procedure TServer.Execute(AContext: TIdContext);
var
  req: string;
  JSON, Auth : TJSONObject;
  action : string;
  sUsername, sUID : string;
  sync : TPrintSync;
begin
  req := AContext.Connection.Socket.ReadLn;
  println('incoming', req);

  // Check to see if it is valid json
  JSON := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(req),0));
  if JSON <> nil then
  begin
    // Check if payload is authenticated.
    if JSON.Get('auth') = nil then
    begin
      println('client', '401 Unauthorized, request has no auth property');
      // Directly write json string to limit memory and code lines
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"data":{"details":"401 Unauthorized, request has no auth property"}}');
      exit;
    end;

    Auth := TJSONObject(JSON.Get('auth').JsonValue);

    if Auth.Get('uid') = nil then
    begin
      println('client', '401 Unauthorized, request has no uid');
      // Directly write json string to limit memory and code lines
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"data":{"details":"401 Unauthorized, request has no uid"}}');
      exit;
    end;

    sUID := Auth.Get('uid').JsonValue.Value;

    if sUID = '' then
    begin
      println('client', '401 Unauthorized, request has no uid');
      // Directly write json string to limit memory and code lines
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"data":{"details":"401 Unauthorized, request has no uid"}}');
      exit;
    end;

    if Auth.Get('username') = nil then
    begin
      println('client', '400 Bad Payload, request has no username');
      // Directly write json string to limit memory and code lines
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"data":{"details":"400 Bad Payload, request has no username"}}');
      exit;
    end;

    sUsername := Auth.Get('username').JsonValue.Value;

    if sUsername = '' then
    begin
      println('client', '400 Bad Payload, request has no username');
      // Directly write json string to limit memory and code lines
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"data":{"details":"400 Bad Payload, request has no username"}}');
      exit;
    end;

    if JSON.Get('action') <> nil then
    begin
      action := JSON.get('action').JsonValue.Value;

      //{"action":"authenticate","auth":{"username":"Wykerd","uid":"{3AA29A4C-C2FB-48BF-A541-2B93E19FE957}"}}
      // Check which action to execute
      if action = 'authenticate' then
      begin
        ClientLogin(sUsername, sUID, AContext.Connection.Socket);
      end
      // If not in current game actions can't be executed by server
      else if not MatchStr(sUID, FSessions) then
      begin
        println('client', 'An unauthorized user made an request to the server');
        // Directly write json string to limit memory and code lines
        AContext.Connection.Socket.WriteLn('{"action":"error",'+
        '"data":{"details":"401 Unauthorized"}}');
      end
      else
      begin
        if JSON.Get('data') <> nil then
          ProcessAction(action, TJSONObject(JSON.Get('data').JsonValue), sUsername, sUID);
      end;
      // End Check for action //

    end
    else
    begin
      println('client', '400 Bad Payload, requires action property');
      AContext.Connection.Socket.WriteLn('{"action":"error",'+
      '"error-details":"400 Bad Payload, requires action property"}');
      exit;
    end;
  end;

end;

procedure TServer.println(t, s: string);
var
  sync : TPrintSync;
begin
  sync := TPrintSync.Create;
  try
    sync.printstr := '[' + uppercase(t) + '] ' + s;
    sync.memo := Debug;
    sync.Synchronize;
  finally
    sync.Free;
  end;
end;

procedure TServer.ProcessAction(action: string; data: TJSONObject; username, uid : string);
begin
  if action = 'chat' then
  begin
    if data.Get('message') <> nil then
      BroadCast('{"action":"chat","data":{"message":"' + username + ': '
         + data.Get('message').JsonValue.Value + '"}}');
  end;
end;

procedure TServer.RunCommand(CMD: TJSONObject);
begin

end;

procedure TServer.SetDebug(const Value: TRichEdit);
begin
  FDebug := Value;
end;

procedure TServer.Start(Port: integer);
begin
  Self.DefaultPort := Port;
  Active := true;
  println('status', 'Server started');

  // Get the local IP address of the computer for LAN games
  TIdStack.IncUsage;
  try
     println('info', 'Hosting on ' + GStack.LocalAddress + ':' + Inttostr(Port));
  finally
    TIdStack.DecUsage;
  end;

end;

procedure TServer.Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  println('server', AStatusText);
end;

end.
