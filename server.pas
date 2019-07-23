unit server;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCustomTCPServer, IdTCPServer, IdContext,
  ComCtrls, Graphics, SysUtils, IdStack, dbUnit, db, helpers;

type
  TServer = class(TIdTCPServer)
  private
    FDebug: TRichEdit;
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
    procedure RunCommand(cmd : TStrArr);
    procedure ClientLogin(username, uid : string);
    property Debug: TRichEdit read FDebug write SetDebug;
  end;

implementation

{ TServer }

procedure TServer.Broadcast(s: string);
var
  tmpList: TList;
  contexClient: TIdContext;
  nClients: integer;
  i: integer;
begin
  tmpList := Contexts.LockList;

  try
    i := 0;
    for I := 0 to tmpList.Count - 1 do
    begin
      contexClient := tmpList[i];
      contexClient.Connection.IOHandler.WriteLn(s);
    end;
  finally
    Contexts.UnlockList;
  end;
end;

procedure TServer.ClientLogin(username, uid: string);
begin
    if dmDB.tblUsers.Locate('UID', uid, []) then
    begin
      // user found
      println('login', 'User found and logged in');
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
    end;
    BroadCast('JOIN|' + username);
  end;

procedure TServer.Connected(AContext: TIdContext);
begin
  println('client', AContext.Binding.PeerIP + ' - Connected');
end;

constructor TServer.Create(AOwner: TComponent);
begin
  inherited;
  OnExecute := Execute;
  OnConnect := Connected;
  Debug := TRichEdit.Create(nil);
end;

procedure TServer.Disconnected(AContext: TIdContext);
begin
  println('client', AContext.Binding.PeerIP + ' - Disconnected');
end;

procedure TServer.Execute(AContext: TIdContext);
var
  req: string;
begin
  req := AContext.Connection.Socket.ReadLn;
  println('incoming', req);
  //
end;

procedure TServer.println(t, s: string);
begin
  Debug.SelAttributes.Color := clRed;
  Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
end;

procedure TServer.RunCommand(cmd: TStrArr);
var
  i : integer;
begin
  if length(cmd) < 2 then
    exit;

  if cmd[1] = 'login' then
  begin
    if length(cmd) <> 4 then
    begin
      println('warning', 'Invalid parameters');
      exit;
    end;
    ClientLogin(cmd[2], cmd[3]);
  end;
end;

procedure TServer.SetDebug(const Value: TRichEdit);
begin
  FDebug := Value;
end;

procedure TServer.Start(Port: integer);
begin
  Self.DefaultPort := Port;
  Active := true;
  println('status', 'server started');
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
