unit gameLogic;

interface

uses
  Windows, DBXJSON, Math, helpers, serverSessions, gameState;

type
  TBroadcastEvent = procedure (s: string) of object;

  TGameLogic = class
  private
    FOnBroadcast: TBroadcastEvent;
    FPlayerSessions: array [0..1] of TClientSession;
    FState: TGameState;
    procedure SetOnBroadcast(const Value: TBroadcastEvent);
    procedure SetState(const Value: TGameState);
  published
    constructor Create(Player1, Player2: TClientSession);
    property State : TGameState read FState write SetState;
    property OnBroadcast : TBroadcastEvent read FOnBroadcast write SetOnBroadcast;
    procedure Init;
  end;

implementation

{ TGameLogic }

constructor TGameLogic.Create(Player1, Player2: TClientSession);
begin
  FPlayerSessions[0] := Player1;
  FPlayerSessions[1] := Player2;
  State := TGameState.Create;
end;

procedure TGameLogic.Init;
begin
  State.BuildStateFromDecks(FPlayerSessions[0].Deck, FPlayerSessions[1].Deck);

  FPlayerSessions[0].Socket.Writeln('{"action":"game-start","data":' + State.ToJSON(0).ToString + '}');
  FPlayerSessions[1].Socket.Writeln('{"action":"game-start","data":' + State.ToJSON(1).ToString + '}');
end;

procedure TGameLogic.SetOnBroadcast(const Value: TBroadcastEvent);
begin
  FOnBroadcast := Value;
end;

procedure TGameLogic.SetState(const Value: TGameState);
begin
  FState := Value;
end;

end.
