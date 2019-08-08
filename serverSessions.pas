unit serverSessions;

interface

type
  TClientSession = class
  private
    FUID: string;
    FSessionID: string;
    FUsername: string;
    procedure SetSessionID(const Value: string);
    procedure SetUID(const Value: string);
    procedure SetUsername(const Value: string);
  published
    property UID : string read FUID write SetUID;
    property Username : string read FUsername write SetUsername;
    property SessionID : string read FSessionID write SetSessionID;
  end;


  TSessionsArr = array of TClientSession;

function GetUserFromUID (UID: string; Sessions: TSessionsArr): integer;
function GetUserFromSID (SID: string; Sessions: TSessionsArr): integer;

implementation

{ TClientSession }

procedure TClientSession.SetSessionID(const Value: string);
begin
  FSessionID := Value;
end;

procedure TClientSession.SetUID(const Value: string);
begin
  FUID := Value;
end;

procedure TClientSession.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

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
