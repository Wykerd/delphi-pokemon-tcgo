unit server;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCustomTCPServer, IdTCPServer, IdContext,
  ComCtrls, Graphics, SysUtils, IdStack, dbUnit, db, helpers, DBXJSON,
  IdIOHandlerSocket, StrUtils, Windows, serverConfig, serverSessions, gameLogic;

type
  TServer = class(TIdTCPServer)
  private
    FDebug: TRichEdit;
    FSessions : TSessionsArr;
    FInGameSessions: array [0..1] of TClientSession;
    FConfig: TServerConfig;
    FGameLogic: TGameLogic;
    procedure Execute(AContext: TIdContext);
    procedure Connected(AContext: TIdContext);
    procedure Disconnected(AContext: TIdContext);
    procedure Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure SetDebug(const Value: TRichEdit);
    procedure println(t, s: string);
    procedure SetConfig(const Value: TServerConfig);
    procedure SetGameLogic(const Value: TGameLogic);
  published
    constructor Create(AOwner: TComponent);
    procedure Start;
    procedure Broadcast(s: string);
    procedure UpdateGameQueue;
    procedure StartGame (Sessions: TSessionsArr);
    property Config : TServerConfig read FConfig write SetConfig;
    procedure ProcessAction(action: string; data: TJSONObject; username, uid : string);
    procedure ClientLogin(username, uid : string; callback : TIdIOHandlerSocket);
    property Debug: TRichEdit read FDebug write SetDebug;
    property GameLogic : TGameLogic read FGameLogic write SetGameLogic;
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

  procedure Authenticate;
  var
    sid: TGuid;
    res: HResult;
  begin
    setlength(FSessions, length(FSessions) + 1);

    FSessions[High(FSessions)] := TClientSession.Create;

    res := CreateGUID(sid);
    if res = S_OK then
       FSessions[High(FSessions)].sessionid := GuidToString(sid)
    else
    begin
      // On error
      callback.WriteLn('{"action":"authenticate","data":{"success":"false"}}');
      exit;
    end;

    FSessions[High(FSessions)].uid := uid;
    FSessions[High(FSessions)].username := username;
    FSessions[High(FSessions)].Socket := callback;

    callback.WriteLn('{"action":"authenticate","data":{"success":"true","sid":"'
      + FSessions[High(FSessions)].sessionid + '"}}');

    BroadCast(Format('{"action":"join","data":{"message":"' + Config.ChatFormat.Join + '"}}',
      [username]));

    UpdateGameQueue;
  end;

begin
  dmDB.tblUsers.Open;
  if GetUserFromUID(uid, FSessions) > -1 then
  begin
    println('login', 'Another user with that ID is already authenticated!');
    callback.WriteLn('{"action":"authenticate","data":{"success":"false"}}');
    exit;
  end;
  if dmDB.tblUsers.Locate('UID', uid, []) then
  begin
    // user found
    println('login', 'User found and logged in');
    Authenticate;
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
        Authenticate;
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
    Authenticate;

  end;
  dmDB.tblUsers.Close;
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
  Config := TServerConfig.Create;
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
  sUsername, sUID, sSID : string;
  bAUTH : boolean;
begin
  req := AContext.Connection.Socket.ReadLn;
  println('incoming', req);

  sSID := '';

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

    if Auth.Get('sid') <> nil then sSID := Auth.Get('sid').JsonValue.Value;

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

    bAUTH := true;

    // IS THE SID AND UID MATCHING?
    if (GetUserFromUID(sUID, FSessions) < 0) OR
      (GetUserFromSID(sSID, FSessions) < 0) then bAUTH := false;

    if (GetUserFromUID(sUID, FSessions) > -1) then
    begin
      if (GetUserFromSID(sSID, FSessions) > -1) then
      begin
        if FSessions[GetUserFromUID(sUID, FSessions)].SessionID <> sSID then
        begin
          bAUTH := false;
        end;

      end;
    end;



    if JSON.Get('action') <> nil then
    begin
      action := JSON.get('action').JsonValue.Value;

      // Check which action to execute
      if action = 'authenticate' then
      begin
        ClientLogin(sUsername, sUID, AContext.Connection.Socket);
      end
      // get server info for server list
      else if action = 'info' then
      begin
        AContext.Connection.Socket.WriteLn('{"action":"info","data":' + Config.Listing.ToString + '}');
      end
      // If not in current game actions can't be executed by server
      else if not bAUTH then
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
begin
  // Queue the action to be preformed in the main thread as it is not
  // thread safe.
  TThread.Queue(nil,
    procedure
    begin
      Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
    end);
end;

procedure TServer.ProcessAction(action: string; data: TJSONObject; username, uid : string);
begin
  if action = 'chat' then
  begin
    if data.Get('message') <> nil then
      BroadCast(Format('{"action":"chat","data":{"message":"' + Config.ChatFormat.Chat + '"}}',
        [username, data.Get('message').JsonValue.Value]));
  end;
end;

procedure TServer.SetConfig(const Value: TServerConfig);
begin
  FConfig := Value;
end;

procedure TServer.SetDebug(const Value: TRichEdit);
begin
  FDebug := Value;
end;

procedure TServer.SetGameLogic(const Value: TGameLogic);
begin
  FGameLogic := Value;
end;

procedure TServer.Start;
begin
  Self.DefaultPort := Config.Port;
  Active := true;
  println('status', 'Server started');

  // Get the local IP address of the computer for LAN games
  TIdStack.IncUsage;
  try
     println('info', 'Hosting on ' + GStack.LocalAddress + ':' + Inttostr(Config.Port));
  finally
    TIdStack.DecUsage;
  end;

end;

procedure TServer.StartGame(Sessions: TSessionsArr);
begin
  if Length(Sessions) <> 2 then
  raise Exception.Create('Woops, that''''s an error! Invalid arguments sent to TServer.StartGame. It has to be length 2');

  FInGameSessions[0] := Sessions[0];
  FInGameSessions[1] := Sessions[1];

  GameLogic := TGameLogic.Create(FInGameSessions[0], FInGameSessions[1]);
  GameLogic.OnBroadcast := Broadcast;
  GameLogic.Init;
end;

procedure TServer.Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  println('server', AStatusText);
end;

procedure TServer.UpdateGameQueue;
var
  PlayersReady: TSessionsArr;
  iPlayersReady, iSLength, i : integer;
begin
  SetLength(PlayersReady, 2);
  iPlayersReady := 0;
  iSLength := Length(FSessions);
  i := -1;
  while (i < (iSLength - 1)) and (iPlayersReady < 2) do
  begin
    inc(i);
    if FSessions[i].ready then
    begin
      PlayersReady[iPlayersReady] := FSessions[i];
      inc(iPlayersReady);
    end;
  end;

  if iPlayersReady >= 2 then
  begin
    StartGame(PlayersReady);
  end
  else
    Broadcast('{"action":"game-queue","data":{"status":"waiting_for_players"}}');
end;

end.
