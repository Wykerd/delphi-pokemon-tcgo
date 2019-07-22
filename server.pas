unit server;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCustomTCPServer, IdTCPServer, IdContext,
  ComCtrls, Graphics, SysUtils;

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

procedure TServer.Connected(AContext: TIdContext);
begin
  println('client', AContext.Binding.IP + ' - Connected');
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
  println('client', AContext.Binding.IP + ' - Disconnected');
end;

procedure TServer.Execute(AContext: TIdContext);
var
  req: string;
begin
  req := AContext.Connection.Socket.ReadLn;
  println('incoming', req);
end;

procedure TServer.println(t, s: string);
begin
  Debug.SelAttributes.Color := clRed;
  Debug.Lines.Add('[' + uppercase(t) + '] ' + s);
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
end;

procedure TServer.Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  println('server', AStatusText);
end;

end.
