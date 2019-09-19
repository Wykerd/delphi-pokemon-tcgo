unit gameLogic;

interface

uses
  Windows, DBXJSON, Math, helpers, serverSessions, gameState, cardData, cardDeck;

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
var
  I: Integer;
  plr1_has_basic, plr2_has_basic : boolean;
  deck1_full, deck2_full : TCardDeck;
begin
  State.BuildStateFromDecks(FPlayerSessions[0].Deck, FPlayerSessions[1].Deck);

  plr1_has_basic := false;
  plr2_has_basic := false;

  deck1_full := State.Deck1;
  deck2_full := State.Deck2;

  while not (plr1_has_basic AND plr2_has_basic) do
  begin
    State.Deck1 := deck1_full;
    State.Deck2 := deck2_full;

    State.Deck1.Shuffle;
    State.Deck2.Shuffle;

    // do pulling of cards
    for I := 0 to 6 do
    begin
      setlength(State.Deck1.Hand, length(State.Deck1.Hand) + 1);
      State.Deck1.Hand[high(State.Deck1.Hand)] := State.Deck1.Deck[high(State.Deck1.Deck)];

      if TCardRecord(State.Deck1.Hand[high(State.Deck1.Hand)]).CardType = ctPokemon then
        if TPokemonData(TCardRecord(State.Deck1.Hand[high(State.Deck1.Hand)]).data).stage = 0 then
          plr1_has_basic := true;

      setlength(State.Deck1.Deck, length(State.Deck1.Deck) - 1);

      setlength(State.Deck2.Hand, length(State.Deck2.Hand) + 1);
      State.Deck2.Hand[high(State.Deck2.Hand)] := State.Deck2.Deck[high(State.Deck2.Deck)];

      if TCardRecord(State.Deck2.Hand[high(State.Deck2.Hand)]).CardType = ctPokemon then
        if TPokemonData(TCardRecord(State.Deck2.Hand[high(State.Deck2.Hand)]).data).stage = 0 then
          plr2_has_basic := true;

      setlength(State.Deck2.Deck, length(State.Deck2.Deck) - 1);
    end;
  end;

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
