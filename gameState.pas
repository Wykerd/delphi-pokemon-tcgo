unit gameState;

interface

uses
  DBXJSON, cardDeck;

type
  TGameState = class
    constructor Create; override;
    procedure BuildStateFromDecks (plrOneDeck, plrTwoDeck : TCardDeck);
    function ToJSON(player: integer) : TJSONObject;
    function UpdatePayload: TJSONObject;
    procedure Flush;
  end;

implementation

{ TGameState }

procedure TGameState.BuildStateFromDecks(plrOneDeck, plrTwoDeck: TCardDeck);
begin

end;

constructor TGameState.Create;
begin
  inherited;

end;

procedure TGameState.Flush;
begin

end;

function TGameState.ToJSON(player: integer): TJSONObject;
begin

end;

function TGameState.UpdatePayload: TJSONObject;
begin

end;

end.
