unit cardDeck;

interface

uses
  DBXJSON, cardData, helpers, sysutils;

type
  TCardDeck = class (TObject)
  published
    constructor CreateFromJSON (JSON : TJSONObject);
  public
    Deck: array of TCardRecord;
    PrizeCards: array of TCardRecord;
    Active: TCardRecord;
    Bench: array of TCardRecord;
    Discard: array of TCardRecord;
    Hand: array of TCardRecord;
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

end.
