unit cardDeck;

interface

uses
  System.JSON, cardData, helpers, sysutils;

type
  TCardDeck = class (TObject)
  published
    constructor CreateFromJSON (JSON : TJSONObject);
    procedure Shuffle;
  public
    Deck: TArray<TCardRecord>;
    PrizeCards: TArray<TCardRecord>;
    Active: TCardRecord;
    Bench: TArray<TCardRecord>;
    Discard: TArray<TCardRecord>;
    Hand: TArray<TCardRecord>;
  end;

implementation


{ TCardDeck }

constructor TCardDeck.CreateFromJSON(JSON: TJSONObject);
var
  cards: TJSONArray;
  I: Integer;
begin
  inherited Create;
  // {"cards":[19,19,19,19,19,18,18,18,18],"name":"Haymaker Starter","_id":"default"}
  cards := TJSONArray(JSON.Get('cards').JsonValue);
  for I := 0 to cards.Size - 1 do
  begin
    setlength(Deck, i + 1);
    Deck[i] := TCardRecord.CreateFromID(strtoint(cards.Get(i).Value));
  end;
  SetLength(PrizeCards, 0);
  SetLength(bench, 0);
  SetLength(discard, 0);
  SetLength(hand, 0);
end;

procedure CardSwap(var a,b : TCardRecord);
var
  temp : TCardRecord;
begin
  temp := a;
  a := b;
  b := temp;
end;

procedure TCardDeck.Shuffle;
var
  I, J, iR: Integer;
begin
  for I := 0 to Length(Deck) - 1 do
  begin
    iR := random(Length(Deck));
    CardSwap(Deck[i], Deck[iR]);
  end;
end;

end.
