unit gameUI;

interface
// Page 148
uses
  Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, ExtCtrls, UIContainer;

type
  TChatEvent = procedure (s : string) of object;

  TGameUI = class (TUIContainer)
  private
    FState: TJSONObject;
    FOnChat: TChatEvent;
    FChatMemo: TMemo;
    procedure SetOnChat(const Value: TChatEvent);
    procedure SetState(const Value: TJSONObject);
    procedure OpenChat(sender: Tobject);
    procedure SetChatMemo(const Value: TMemo);
  published
    constructor Create (AOwner: TComponent); override;
    property State : TJSONObject read FState write SetState;
    property OnChat : TChatEvent read FOnChat write SetOnChat;
    procedure IncomingChat (s : string);
    property ChatMemo : TMemo read FChatMemo write SetChatMemo;
  end;

implementation

{ TGameUI }

constructor TGameUI.Create(AOwner: TComponent);
begin
  inherited;
  OnClick := OpenChat;
  ChatMemo := TMemo.Create(self);
  ChatMemo.Align := alBottom;
  ChatMemo.Parent := self;
  ChatMemo.ReadOnly := true;
end;

procedure TGameUI.IncomingChat(s: string);
begin
  ChatMemo.Lines.Add(s);
end;

procedure TGameUI.OpenChat(sender: Tobject);
begin
  OnChat(Inputbox('Chat', 'Enter message', ''));
end;

procedure TGameUI.SetChatMemo(const Value: TMemo);
begin
  FChatMemo := Value;
end;

procedure TGameUI.SetOnChat(const Value: TChatEvent);
begin
  FOnChat := Value;
end;

procedure TGameUI.SetState(const Value: TJSONObject);
begin
  FState := Value;
end;

end.
