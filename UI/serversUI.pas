unit serversUI;

interface

uses
  UIContainer, Classes, DBXJSON, helpers, ExtCtrls;

type
  TServerListing = class (TPanel)
    constructor CreateWithState (AOwner : TComponent; sName, sLName,
      sMotd, sHost : string; iPort : integer);
  end;

  TServersUI = class (TUIContainer)
  private
    FServers: TJSONArray;
    Listings : TServerListing;
    procedure SetServers(const Value: TJSONArray);
  published
    constructor Create (AOwner: TComponent); override;
    procedure LoadFromFile(s : string);
    property Servers : TJSONArray read FServers write SetServers;
  end;

implementation

{ TServersUI }

constructor TServersUI.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Server List';
end;

procedure TServersUI.LoadFromFile(s: string);
begin

end;

procedure TServersUI.SetServers(const Value: TJSONArray);
begin
  FServers := Value;
end;

{ TServerListing }

constructor TServerListing.CreateWithState(AOwner: TComponent; sName, sLName,
  sMotd, sHost: string; iPort: integer);
begin
  inherited Create (AOwner);

end;

end.