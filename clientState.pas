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

end.
