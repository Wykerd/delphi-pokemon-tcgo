unit serverSessions;

interface

uses
  IdIOHandlerSocket, cardDeck, System.JSON;

type
  TClientSession = class
  private
    FUID: string;
    FSessionID: string;
    FUsername: string;
    FSocket: TIdIOHandlerSocket;
    FReady: boolean;
    FDeck: TCardDeck;
    FDecks: TJSONArray;
    procedure SetSessionID(const Value: string);
    procedure SetUID(const Value: string);
    procedure SetUsername(const Value: string);
    procedure SetSocket(const Value: TIdIOHandlerSocket);
    procedure SetReady(const Value: boolean);
    procedure SetDeck(const Value: TCardDeck);
    procedure SetDecks(const Value: TJSONArray);
  published
    constructor Create;
    property UID : string read FUID write SetUID;
    property Username : string read FUsername write SetUsername;
    property SessionID : string read FSessionID write SetSessionID;
    property Socket : TIdIOHandlerSocket read FSocket write SetSocket;
    property Ready: boolean read FReady write SetReady;
    property Deck : TCardDeck read FDeck write SetDeck;
    property Decks : TJSONArray read FDecks write SetDecks;
    procedure LoadDeck(index: integer);
  end;


  TSessionsArr = TArray<TClientSession>;

function GetUserFromUID (UID: string; Sessions: TSessionsArr): integer;
function GetUserFromSID (SID: string; Sessions: TSessionsArr): integer;

implementation

{ TClientSession }

constructor TClientSession.Create;
begin
  FReady := false;
end;

procedure TClientSession.LoadDeck(index: integer);
begin
  //
end;

procedure TClientSession.SetDeck(const Value: TCardDeck);
begin
  FDeck := Value;
end;

procedure TClientSession.SetDecks(const Value: TJSONArray);
begin
  FDecks := Value;
end;

procedure TClientSession.SetReady(const Value: boolean);
begin
  FReady := Value;
end;

procedure TClientSession.SetSessionID(const Value: string);
begin
  FSessionID := Value;
end;

procedure TClientSession.SetSocket(const Value: TIdIOHandlerSocket);
begin
  FSocket := Value;
end;

procedure TClientSession.SetUID(const Value: string);
begin
  FUID := Value;
end;

procedure TClientSession.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

// returns the index!
function GetUserFromUID (UID: string; Sessions: TSessionsArr): integer;
var
  i : integer;
begin
  if UID = '' then exit(-1);

  i := 0;
  result := -1;
  while i <= High(Sessions) do
  begin
    if Sessions[i].UID = UID then exit(i);
    inc(i);
  end;
end;

function GetUserFromSID (SID: string; Sessions: TSessionsArr): integer;
var
  i : integer;
begin
  if SID = '' then exit(-1);

  i := 0;
  result := -1;
  while i <= High(Sessions) do
  begin
    if Sessions[i].SessionID = SID then exit(i);
    inc(i);
  end;
end;

end.
