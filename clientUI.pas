unit clientUI;

interface
// Page 148
uses
  Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls;

type
  TChatEvent = procedure (s : string) of object;

  TClientUI = class (TForm)
  private
    FState: TJSONObject;
    FOnChat: TChatEvent;
    procedure SetOnChat(const Value: TChatEvent);
    procedure SetState(const Value: TJSONObject);
    procedure OpenChat(sender: Tobject);
  published
    constructor CreateNew (AOwner: TComponent; Dummy: integer); override;
    property State : TJSONObject read FState write SetState;
    property OnChat : TChatEvent read FOnChat write SetOnChat;
    procedure IncomingChat (s : string);
  end;

implementation

{ TClientUI }

constructor TClientUI.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  OnClick := OpenChat;
end;

procedure TClientUI.IncomingChat(s: string);
begin
  Showmessage(s);
end;

procedure TClientUI.OpenChat(sender: Tobject);
begin
  OnChat(Inputbox('Chat', 'Enter message', ''));
end;

procedure TClientUI.SetOnChat(const Value: TChatEvent);
begin
  FOnChat := Value;
end;

procedure TClientUI.SetState(const Value: TJSONObject);
begin
  FState := Value;
end;

end.
