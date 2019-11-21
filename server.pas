unit server;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCustomTCPServer, IdTCPServer, IdContext,
  ComCtrls, Graphics, SysUtils, IdStack, dbUnit, db, helpers, System.JSON,
  IdIOHandlerSocket, StrUtils, Windows, serverConfig, serverSessions, gameLogic,
  IdHTTP, HTTPApp, cardDeck, versions;

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
  public
    InGame: boolean;
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
  i : boolean;

  procedure Authenticate;
  begin
    TAnonymousThread.Create(procedure
      var
        sid: TGuid;
        idHttp : TIdHTTP;
        api_res : string;
        err, json, auth_res, auth_res_data, tmp, deckObj : TJSONObject;
        names, decks : TJSONArray;
        i : integer;
      begin
        // Get the client data from the api
        idHttp := TIdHTTP.Create;
        idHttp.HandleRedirects := true;
        try
          try
            api_res := idHttp.Get(format('%sgetDeckData?uid=%s&user=%s',
              [Config.APIURL,
              String(Httpencode(Config.UID)),
              String(Httpencode(uid))]));
            println('login', api_res);

            // Check the response for decks
            json := TJsonObject(TJSONObject.ParseJSONValue(
                TEncoding.ASCII.GetBytes(StripNonJson(api_res)),0));
            if json <> nil then
            begin
              if json.Exists('data') then
              begin
                decks := TJSONArray(json.Get('data').JsonValue);
                names := TJSONArray.Create;
                if decks <> nil then
                begin
                  for I := 0 to decks.Size - 1 do
                  begin
                    tmp := TJSONObject(decks.Get(i));
                    if tmp.Exists('name') then
                    begin
                      deckObj := TJSONObject.Create;
                      deckObj.AddPair(TJSONPair.Create('name', tmp.Get('name').JsonValue.Value));
                      deckObj.AddPair(TJSONPair.Create('index', inttostr(i)));
                      names.Add(deckObj);
                    end;
                  end; //end for loop to populate names

                  // Create the session
                  setlength(FSessions, length(FSessions) + 1);

                  FSessions[High(FSessions)] := TClientSession.Create;

                  CreateGUID(sid);
                  FSessions[High(FSessions)].sessionid := GuidToString(sid);

                  FSessions[High(FSessions)].uid := uid;
                  FSessions[High(FSessions)].username := username;
                  FSessions[High(FSessions)].Socket := callback;
                  FSessions[High(FSessions)].Decks := decks;
                  // end creating session

                  auth_res_data := TJSONObject.Create;
                  auth_res_data.AddPair(TJSONPair.Create('success', 'true'));
                  auth_res_data.AddPair(TJSONPair.Create('sid', FSessions[High(FSessions)].sessionid));
                  auth_res_data.AddPair(TJSONPair.Create('decks', names));

                  auth_res := TJSONObject.Create;
                  auth_res.AddPair(TJSONPair.Create('action', 'authenticate'));
                  auth_res.AddPair(TJSONPair.Create('data', auth_res_data));

                  callback.WriteLn(auth_res.ToString);

                  BroadCast(Format('{"action":"join","data":{"message":"' + Config.ChatFormat.Join + '"}}',
                    [username]));

                  UpdateGameQueue;
                  //

                end; //end decks check
                names.Free;
              end // json has data
              else
                callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"api_error"}}');
            end // json is defined
            else
              callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"api_error"}}');
          finally
            idHttp.Free;
          end;
        except
          on E : Exception do
          begin
            println('login', 'ERROR: ' + E.ClassName+' error raised, with message : '+E.Message);
            if E is EIdHTTPProtocolException then
            begin
              err := TJsonObject(TJSONObject.ParseJSONValue(
                TEncoding.ASCII.GetBytes(StripNonJson((e as EIdHTTPProtocolException).ErrorMessage)),0));
              println('login', (e as EIdHTTPProtocolException).ErrorMessage);
              if err.Exists('message') then
                callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"' +
                  err.Get('message').JsonValue.Value + '"}}')
              else
                callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"internal_error"}}');
            end
            else
              callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"internal_error"}}');
          end;
        end;
      end).Start;
  end;

begin
  dmDB.tblUsers.Open;
  dmDB.tblUsers.First;
  if GetUserFromUID(uid, FSessions) > -1 then
  begin
    println('login', 'Another user with that ID is already authenticated!');
    callback.WriteLn('{"action":"authenticate","data":{"success":"false","reason":"session_exists"}}');
    exit;
  end;
  if dmDB.tblUsers.Locate('UID', uid, []) then
  begin
    // user found
    println('login', 'User found in database.');
    Authenticate;

    // check that username is up to date on server side
    with dmDB do
    begin
      tblUsers.Edit;
      tblUsers['LastLogin'] := date;
      if tblUsers['UserName'] <> username then
      begin
        tblUsers['UserName'] := username;
        println('login', 'Username did not match database record and has been updated.');
      end;
      tblUsers.Post;
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
      tblUsers['LastLogin'] := date;
      tblUsers.Post;
    end;
    println('login', 'User created.');
    Authenticate;

  end;
  dmDB.tblUsers.Close;
end;

procedure TServer.Connected(AContext: TIdContext);
begin
  println('client', AContext.Binding.PeerIP + ' - Connected');
  AContext.Connection.Socket.WriteLn(
    format('{"action":"preflight","data":{"version":"%s","legacy-support":"%s"'+
    ',"backwards-version":"%s"}}',
    [SRV_SERVER_VERSION, SRV_LEGACY_SUPPORT, SRV_BACKWARDS_COMPATIBILITY]));
end;

constructor TServer.Create(AOwner: TComponent);
begin
  inherited;
  InGame := false;
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
      else if action = 'listing-info' then
      begin
        AContext.Connection.Socket.WriteLn('{"action":"listing-info","data":' + Config.Listing.ToString + '}');
      end
      // If not in current game actions can't be executed by server
      else if not bAUTH then
      begin
        println('client', 'An unauthorized user made an request to the server');
        // Directly write json string to limit memory and code lines
        AContext.Connection.Socket.WriteLn('{"action":"error",'+
        '"data":{"details":"401 Unauthorized"}}');
      end
      else if not json.Exists('data') then
      begin
        AContext.Connection.Socket.writeln('{"action":"error",'+
        '"data":{"details":"no_data"}}');
      end
      else
      begin
        if action = 'game-action' then
        begin
          GameLogic.ProcessGameAction(json, sUID, TJSONObject(JSON.Get('data').JsonValue));
        end

        else if JSON.Get('data') <> nil then
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
      try
        Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
      except
        // ignore exceptions.
      end;
    end);
end;

procedure TServer.ProcessAction(action: string; data: TJSONObject; username, uid : string);
var
  Session: TClientSession;
  temp : Variant;
  jsontemp : TJSONObject;
begin
  Session := FSessions[GetUserFromUID(uid, FSessions)];

  if action = 'chat' then
  begin
    if data.Get('message') <> nil then
      BroadCast(Format('{"action":"chat","data":{"message":"' + Config.ChatFormat.Chat + '"}}',
        [username, data.Get('message').JsonValue.Value]));
  end

  else if action = 'game-ready' then
  begin
    if Session.Deck <> nil then
    begin
      Session.Ready := true;
      UpdateGameQueue;
      Session.Socket.WriteLn('{"action":"game-ready","data":{"success":"true"}}');
      exit;
    end
    else
      Session.Socket.WriteLn('{"action":"game-ready","data":{"success":"false","reason":"no_deck_used"}}');
  end

  else if action = 'game-use-deck' then
  begin
    if data.Exists('index') then
    begin
      try
        try
          temp := strtoint(data.Get('index').JsonValue.Value);
        finally
          if (temp < session.Decks.Size) and (temp > -1) then
          begin
            jsontemp := TJSONObject(Session.Decks.Get(temp));
            try
              try
                Session.Deck := TCardDeck.CreateFromJSON(jsontemp);
              finally
                Session.Ready := false;
                Session.Socket.WriteLn('{"action":"game-use-deck","data":{"success":"true"}}');
              end;
            except
              on e: exception do
              begin
                println('DECK BUILD', 'ERROR: ' + e.Message);
                Session.Socket.WriteLn('{"action":"game-use-deck","data":{"success":"false","reason":"deck_build_failed"}}');
              end;
            end;
          end
          else
          begin
            Session.Socket.WriteLn('{"action":"game-use-deck","data":{"success":"false","reason":"out_of_range"}}');
          end;
        end;
      except
        Session.Socket.WriteLn('{"action":"game-use-deck","data":{"success":"false","reason":"invalid_request"}}');
      end;
    end;
  end; // end if game-use-deck

  // UPDATE QUEUE
  UpdateGameQueue;
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
var
  idHttp: TIdHTTP;
  api_res: string;
  json, err: TJSONObject;
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

  idHttp := TIdHTTP.Create;
  idHttp.HandleRedirects := true;
  try
    try
      api_res := idHttp.Get(format('%sregisterServer?uid=%s&motd=%s&name=%s&'+
        'custom_name=%s&git_repo=%s&website=%s&version_name=%s&'+
        'spec_api_version=%s&spec_server_version=%s&spec_client_version=%s&'+
        'spec_legacy_support=%s&spec_backwards_compatibility=%s',
        [Config.APIURL,
        String(Httpencode(Config.UID)),
        String(Httpencode(Config.Listing.Get('motd').JsonValue.Value)),
        String(Httpencode(Config.Listing.Get('name').JsonValue.Value)),
        String(Httpencode(SRV_CUSTOM_NAME)),
        String(Httpencode(SRV_GIT_REPO)),
        String(Httpencode(SRV_WEBSITE)),
        String(Httpencode(VERSION_NAME)),
        String(Httpencode(SRV_API_VERSION)),
        String(Httpencode(SRV_SERVER_VERSION)),
        String(Httpencode(SRV_CLIENT_VERSION)),
        String(Httpencode(SRV_LEGACY_SUPPORT)),
        String(Httpencode(SRV_BACKWARDS_COMPATIBILITY))]));

      // Check the response for decks
      json := TJsonObject(TJSONObject.ParseJSONValue(
          TEncoding.ASCII.GetBytes(StripNonJson(api_res)),0));

      println('api', 'Registration OK');

      if json.Exists('warning') then
      begin
        Debug.SelAttributes.Style := [fsBold];
        println('api', json.Get('warning').JsonValue.Value);
      end;

    finally

    end;
  except
    on E: Exception do
    begin
      println('api', 'An error occured during server API registration');
    end;
  end;
  idHttp.Free;

end;

procedure TServer.StartGame(Sessions: TSessionsArr);
begin
  if Length(Sessions) <> 2 then
  raise Exception.Create('Woops, that''''s an error! Invalid arguments sent to TServer.StartGame. It has to be length 2');

  ingame := true;

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
  if ingame then exit;

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
