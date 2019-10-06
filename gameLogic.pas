unit gameLogic;

interface

uses
  Windows, Math, helpers, serverSessions, gameState, cardData, cardDeck;

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
begin
  State.BuildStateFromDecks(FPlayerSessions[0].Deck, FPlayerSessions[1].Deck);

  plr1_has_basic := false;
  plr2_has_basic := false;

  while not (plr1_has_basic AND plr2_has_basic) do
  begin
    // Put hand back to deck
    for I := Low(State.Deck1.Hand) to High(State.Deck1.Hand) do
    begin
      SetLength(State.Deck1.Deck, Length(State.Deck1.Deck) + 1);
      State.Deck1.Deck[High(State.Deck1.Deck)] := State.Deck1.Hand[i];
    end;

    SetLength(State.Deck1.Hand, 0);

    for I := Low(State.Deck2.Hand) to High(State.Deck2.Hand) do
    begin
      SetLength(State.Deck2.Deck, Length(State.Deck2.Deck) + 1);
      State.Deck2.Deck[High(State.Deck2.Deck)] := State.Deck2.Hand[i];
    end;

    SetLength(State.Deck2.Hand, 0);

    // Shuffle the deck;
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

    // DEBUG ONLY! //

      State.Deck1.Active := State.Deck1.Deck[0];
      State.Deck2.Active := State.Deck2.Deck[0];

      setlength(State.Deck1.Discard, 1);
      State.Deck1.Discard[0] := State.Deck1.Deck[0];
      setlength(State.Deck2.Discard, 1);
      State.Deck2.Discard[0] := State.Deck2.Deck[0];

      setlength(State.Deck1.Bench, 1);
      State.Deck1.Bench[0] := State.Deck1.Deck[1];
      setlength(State.Deck2.Bench, 1);
      State.Deck2.Bench[0] := State.Deck2.Deck[1];

  end;

  // Pull the prize cards
  for I := 0 to 5 do
  begin
    setlength(State.Deck1.PrizeCards, i + 1);
    setlength(State.Deck2.PrizeCards, i + 1);

    State.Deck1.PrizeCards[i] := State.Deck1.Deck[high(State.Deck1.Deck)];
    setlength(State.Deck1.Deck, length(State.Deck1.Deck) - 1);

    State.Deck2.PrizeCards[i] := State.Deck2.Deck[high(State.Deck2.Deck)];
    setlength(State.Deck2.Deck, length(State.Deck2.Deck) - 1);
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
