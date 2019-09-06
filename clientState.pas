unit clientState;

interface

type
  TClientState = class
  private
    FReady: boolean;
    procedure SetReady(const Value: boolean);
  published
    constructor Create;
    property Ready : boolean read FReady write SetReady;
    // TCardModel
  end;

function DecodeAuthReason (s: string) : string;

implementation

{ TClientState }

constructor TClientState.Create;
begin
  Ready := false;
end;

procedure TClientState.SetReady(const Value: boolean);
begin
  FReady := Value;
end;

// General //
function DecodeAuthReason (s: string) : string;
begin
  result := 'Unknown reason. Is the server modded? Make sure it complies with the spec';
  if s = 'session_exists' then result := 'There is already a session issued to this user'
  else if s = 'user_undefined' then result := 'You are not registered with the API'
  else if s = 'no_decks' then result := 'You have not made any decks in the API'
  else if s = 'internal_error' then result := 'Internal server error';
end;

end.
