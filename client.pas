unit client;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdContext, IdThreadComponent, ComCtrls,
  Graphics, SysUtils;

type
  TClient = class (TIdTCPClient)
  private
    FDebug: TRichEdit;
    FIdThread : TIdThreadComponent;
    procedure SetDebug(const Value: TRichEdit);
    procedure Run (Sender: TIdThreadComponent);
    procedure println (t, s : string);
    procedure Connected(Sender: TObject);
  published
    constructor Create (AOwner: TComponent);
    procedure Start (Host: string; Port: integer);
    property Debug : TRichEdit read FDebug write SetDebug;
  end;

implementation

{ TClient }

procedure TClient.Connected(Sender: TObject);
begin
  FIdThread.Active := true;
  println('status', 'Connected');
end;

constructor TClient.Create(AOwner: TComponent);
begin
  inherited;
  FIdThread := TIdThreadComponent.Create(AOwner);
  FIdThread.OnRun := Run;
  OnConnected := Connected;
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

procedure TClient.SetDebug(const Value: TRichEdit);
begin
  FDebug := Value;
end;

procedure TClient.Start(Host: string; Port: integer);
begin
  Self.Host := Host;
  Self.Port := Port;
  Connect;
end;

end.
