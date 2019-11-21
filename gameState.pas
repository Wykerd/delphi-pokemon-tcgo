unit gameState;

interface

uses
  System.JSON, cardDeck, Consts, sysutils, cardData, Dialogs;

type
  TGameState = class (TObject)
  published
    procedure BuildStateFromDecks (plrOneDeck, plrTwoDeck : TCardDeck);
    function ToJSON(player: byte) : TJSONObject;
    procedure StartTurn;
    procedure BuildTurnStage;
  public
    Deck1: TCardDeck;
    Deck2 : TCardDeck;
    turn : byte;
    stage: string;
    turn_retreat: boolean;
    turn_energy: boolean;
  end;

implementation

{ TGameState }

procedure TGameState.BuildStateFromDecks(plrOneDeck, plrTwoDeck: TCardDeck);
begin
  Deck1 := plrOneDeck;
  Deck2 := plrTwoDeck;

  // Shuffle the cards
  Deck1.shuffle;
  Deck2.shuffle;

  //
  turn := 0;
  stage := 'init';
end;

procedure TGameState.BuildTurnStage;
begin
  stage := 'card_';
  if turn_retreat and turn_energy then stage := stage + 'retreat+energy'
  else if turn_retreat then stage := stage + 'retreat'
  else if turn_energy then stage := stage + 'energy';
end;

procedure TGameState.StartTurn;
begin
  turn_retreat := true;
  turn_energy := true;
  BuildTurnStage;
end;

function TGameState.ToJSON(player: byte): TJSONObject;
var
  plr, opnt : TCardDeck;
  state : TJSONObject;
  arr : TJSONArray;
  I: Integer;
begin
  if player = 0 then
  begin
    plr := Deck1;
    opnt := Deck2;
  end
  else
  begin
    plr := Deck2;
    opnt := Deck1;
  end;

  result := TJSONObject.Create;

  // Player state //
  state := TJSONObject.Create;
  if length(plr.Deck) <> 0 then
    state.AddPair(TJSONPair.Create('deck', TCardRecord(plr.deck[high(plr.deck)]).tojson));

  state.AddPair(TJSONPair.Create('deck-length', inttostr(length(plr.Deck))));

  state.addpair(TJSONPair.Create('prize-cards', inttostr(length(plr.prizecards))));

  arr := TJSONArray.create;
  for I := 0 to length(plr.bench) - 1 do
    arr.Add(TCardRecord(plr.bench[i]).tojson);
  state.addpair(TJSONPair.create('benched-cards', arr));

  if plr.active <> nil then
    state.addpair(TJSONPair.create('active-card', plr.active.tojson));

  arr := TJSONArray.create;
  for I := 0 to length(plr.hand) - 1 do
    arr.Add(TCardRecord(plr.hand[i]).tojson);
  state.addpair(TJSONPair.create('hand', arr));

  arr := TJSONArray.create;
  for I := 0 to length(plr.discard) - 1 do
    arr.Add(TCardRecord(plr.discard[i]).tojson);
  state.addpair(TJSONPair.create('discard', arr));
  // end player state //

  result.addpair(TJSONPair.create('player', state));

  // oponent state //
  state := TJSONObject.Create;
  state.AddPair(TJSONPair.Create('deck-length', inttostr(length(opnt.Deck))));

  state.addpair(TJSONPair.Create('prize-cards', inttostr(length(opnt.prizecards))));

  arr := TJSONArray.create;
  for I := 0 to length(opnt.bench) - 1 do
    arr.Add(TCardRecord(opnt.bench[i]).tojson);
  state.addpair(TJSONPair.create('benched-cards', arr));

  if opnt.active <> nil then
    state.addpair(TJSONPair.create('active-card', opnt.active.tojson));

  state.AddPair(TJSONPair.Create('hand', inttostr(length(opnt.hand))));

  arr := TJSONArray.create;
  for I := 0 to length(opnt.discard) - 1 do
    arr.Add(TCardRecord(opnt.discard[i]).tojson);
  state.addpair(TJSONPair.create('discard', arr));
  // end oponent state //

  result.addpair(TJSONPair.create('oponent', state));


  // gameplay state //
  state := TJSONObject.Create;
  if turn = player then
  begin
    state.addpair(TJSONPair.create('turn', 'player'));
  end
  else
  begin
    state.addpair(TJSONPair.create('turn', 'oponent'));
  end;
  state.addpair(TJSONPair.create('stage', stage));
  // end gameplay state //

  result.addpair(TJSONPair.create('gameplay', state));
end;

end.
