unit serverConfig;

interface

type
  TChatFormat = record
    Chat : string;
    Join : string;
  end;

  TServerConfig = class
  private
    FChatFormat: TChatFormat;
    procedure SetChatFormat(const Value: TChatFormat);
  published
    constructor Create (APath : string);
    property ChatFormat : TChatFormat read FChatFormat write SetChatFormat;
  end;

implementation

{ TServerConfig }

constructor TServerConfig.Create(APath: string);
begin

end;

procedure TServerConfig.SetChatFormat(const Value: TChatFormat);
begin
  FChatFormat := Value;
end;

end.
