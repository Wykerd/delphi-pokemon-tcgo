unit serverConfig;

interface

uses
  DBXJSON, SysUtils, helpers, ExtCtrls;

type
  TChatFormat = record
  private
    FJoin: string;
    FChat: string;
    procedure SetChat(const Value: string);
    procedure SetJoin(const Value: string);
  public
    property Chat : string read FChat write SetChat;
    property Join : string read FJoin write SetJoin;
  end;

  TServerConfig = class
  private
    FChatFormat: TChatFormat;
    FWhitelist: boolean;
    FListing: TJSONObject;
    FPort: integer;
    FMaxPlayers: integer;
    procedure SetChatFormat(const Value: TChatFormat);
    procedure SetListing(const Value: TJSONObject);
    procedure SetMaxPlayers(const Value: integer);
    procedure SetPort(const Value: integer);
    procedure SetWhitelist(const Value: boolean);
  published
    constructor Create;
    property ChatFormat : TChatFormat read FChatFormat write SetChatFormat;
    property Port : integer read FPort write SetPort;
    property MaxPlayers : integer read FMaxPlayers write SetMaxPlayers;
    property Whitelist : boolean read FWhitelist write SetWhitelist;
    property Listing : TJSONObject read FListing write SetListing;
    procedure LoadFromFile (APath : string);
  end;

implementation

{ TServerConfig }

constructor TServerConfig.Create;
begin
  // Defaults
  ChatFormat.Chat := '<%s> %s';
  ChatFormat.Join := '%s joined the game';

  Port := 8080;
  MaxPlayers := -1;
  Whitelist := false;
  Listing := TJSONObject.Create;
  Listing.AddPair(TJSONPair.Create('motd', 'A Gaming Server'));
  Listing.AddPair(TJSONPair.Create('name', 'New Server'));
end;

procedure TServerConfig.LoadFromFile(APath: string);
var
  json, tmp: TJSONObject;
  tF : textfile;
  dat, s : string;
begin
  assignfile(tf, APath);

  try
    try
      reset(tF);
    except
      raise Exception.CreateFmt('Could not load server config %s', [QuotedStr(APath)]);
    end;
  finally
    while not eof(tf) do
    begin
      Readln(tf, s);
      dat := dat + s;
    end;

    json := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(dat)),0));

    if json = nil then
    begin
      raise Exception.CreateFmt('The config at %s is invalid JSON', [QuotedStr(APath)]);
    end;

    with json do
    begin
      // Make sure required fields are found
      if (get('chat-format') = nil) or (get('join-format') = nil) or
        (get('port') = nil) or (get('max-players') = nil) or
        (get('whitelist') = nil) or (get('listing') = nil) then
        raise Exception.CreateFmt('The config at %s is missing required fields',
          [QuotedStr(APath)])
      else
      begin
        ChatFormat.Chat := get('chat-format').JsonValue.Value;
        ChatFormat.Join := get('join-format').JsonValue.Value;

        Port := strtoint(get('port').JsonValue.Value);
        MaxPlayers := strtoint(get('max-players').JsonValue.Value);

        if get('whitelist').JsonValue.Value = 'true' then
          Whitelist := true
        else
          Whitelist := false;

        tmp := TJSONObject(get('listing').JsonValue);

        // Make sure required fields are found
        if (tmp.Get('motd') = nil) or (tmp.Get('name') = nil) then
          Listing := tmp;
      end;
    end;
    
  end;
end;

procedure TServerConfig.SetChatFormat(const Value: TChatFormat);
begin
  FChatFormat := Value;
end;

procedure TServerConfig.SetListing(const Value: TJSONObject);
begin
  FListing := Value;
end;

procedure TServerConfig.SetMaxPlayers(const Value: integer);
begin
  FMaxPlayers := Value;
end;

procedure TServerConfig.SetPort(const Value: integer);
begin
  FPort := Value;
end;

procedure TServerConfig.SetWhitelist(const Value: boolean);
begin
  FWhitelist := Value;
end;

{ TChatFormat }

procedure TChatFormat.SetChat(const Value: string);
begin
  FChat := Value;
end;

procedure TChatFormat.SetJoin(const Value: string);
begin
  FJoin := Value;
end;

end.
