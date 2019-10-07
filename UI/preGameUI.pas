unit preGameUI;

interface

uses
  Windows, Messages, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, System.JSON,
  UIContainer, helpers, SysUtils, UIButton, Forms, Math;

type
  TDeckButton =  class (TUIButton)
  private
    FInternalState: Variant;
    procedure SetInternalState(const Value: Variant);
  published
    property InternalState: Variant read FInternalState write SetInternalState;
  end;

  TSelectCards = class (TUIContainer)
  private
    FState: TJSONArray;
    // UI elements
    // uni-directional data flow to render components -
    // so delete whole component and rerender -
    // this way the individual decks aren't tracked!
    Decks: TScrollBox;
    TitleLabel : TLabel;
    ReadyButton : TUIButton;
    FPushReady: TGenericEvent;
    FSelected: boolean;
    procedure HandleDeckPick(Sender: TObject);
    procedure HandleReady(Sender: TObject);
    procedure HandleResize(Sender: TObject);
    //
    procedure SetState(const Value: TJSONArray);
    procedure SetPushReady(const Value: TGenericEvent);
    procedure HandleAfterResize(var Message: TMessage); message WM_USER +111;
    procedure SetSelected(const Value: boolean);
  published
    constructor Create(AOwner: TComponent); override;
    property State : TJSONArray read FState write SetState;
    property OnReady : TGenericEvent read FPushReady write SetPushReady;
    property Selected: boolean read FSelected write SetSelected;
    procedure RenderState;
  public
    OnDeckChange: TGenericEvent<integer>;
  end;

  TPreGameUI = class (TUIContainer)
  private
    FAuthenticated: boolean;
    FReady: boolean;
    FConnected: boolean;
    FDecks: TJSONArray;
    procedure SetAuthenticated(const Value: boolean);
    procedure SetReady(const Value: boolean);
    procedure SetConnected(const Value: boolean);
    procedure SetDecks(const Value: TJSONArray);
  published
    constructor Create(AOwner: TComponent); override;
    property Authenticated : boolean read FAuthenticated write SetAuthenticated;
    property Connected : boolean read FConnected write SetConnected;
    procedure Error(Reason : string);
    procedure Print(s: string);
    property Decks : TJSONArray read FDecks write SetDecks;
    property Ready: boolean read FReady write SetReady;
    procedure ShowDeckSelect;
  public
    Loading: TUIContainer;
    SelectDeck : TSelectCards;
  end;

implementation

{ TPreGameUI }

procedure TPreGameUI.Error(Reason: string);
begin
  loading.Caption := reason;
  SelectDeck.Visible := false;
  loading.Visible := true;
end;

procedure TPreGameUI.Print(s: string);
begin
  loading.Caption := s;
  SelectDeck.Visible := false;
  loading.Visible := true;
end;

constructor TPreGameUI.Create(AOwner: TComponent);
begin
  inherited;
  Loading := TUIContainer.Create(self);
  loading.Parent := self;
  loading.Font.Name := 'Early Gameboy';
  loading.Color := rgb(24, 139, 180);
  Authenticated := false;
  loading.Caption := 'Connecting to server...';

  SelectDeck := TSelectCards.Create(self);
  SelectDeck.Parent := self;
  SelectDeck.Visible := false;
end;

procedure TPreGameUI.SetAuthenticated(const Value: boolean);
begin
  FAuthenticated := Value;
  if value then
  begin
    if Decks = nil then loading.Caption := 'Awaiting state'
    else ShowDeckSelect;
  end;
end;

procedure TPreGameUI.SetConnected(const Value: boolean);
begin
  FConnected := Value;
  if authenticated then
  begin
    if Decks = nil then loading.Caption := 'Awaiting state'
    else ShowDeckSelect;
  end
  else loading.Caption := 'Authenticating with server...';
end;

procedure TPreGameUI.SetDecks(const Value: TJSONArray);
begin
  if Value <> nil then
  begin
    FDecks := Value;
    ShowDeckSelect;
  end
  else Error('Invalid deck array from server');
end;

procedure TPreGameUI.SetReady(const Value: boolean);
begin
  FReady := Value;
  SelectDeck.ReadyButton.Text := 'Ready?';
  SelectDeck.ReadyButton.Enabled := true;
end;

procedure TPreGameUI.ShowDeckSelect;
begin
  loading.Caption := 'Received state';
  loading.Visible := false;
  SelectDeck.Visible := true;
  SelectDeck.State := Decks;
end;

{ TSelectCards }

constructor TSelectCards.Create(AOwner: TComponent);
begin
  inherited;
  
  DoubleBuffered := true;
  color := rgb(148, 180, 197);

  // Build the UI
  Decks := TScrollBox.Create(self);
  Decks.Parent := self;
  Decks.Top := 1;

  with Decks do
  begin
    Align := alClient;
    AlignWithMargins := true;
    Margins.Left := 0;
    Margins.Right := 0;
    Margins.Top := 20;
    Margins.Bottom := 20;
    BevelEdges := [];
    BorderStyle := bsNone;
    Parent := self;
    Color := rgb(238, 156, 139);
  end;

  TitleLabel := TLabel.Create(self);
  TitleLabel.Parent := self;
  TitleLabel.Align := alTop;
  TitleLabel.AlignWithMargins := True;
  TitleLabel.Margins.Top := 20;
  TitleLabel.Caption := 'Click to select the deck to use';
  TitleLabel.Font.Name := 'Early Gameboy';
  TitleLabel.Alignment := taCenter;

  ReadyButton := TUIButton.Create(self);
  ReadyButton.Parent := self;
  ReadyButton.Enabled := false;
  ReadyButton.OnClick := HandleReady;

  OnResize := HandleResize;
end;

procedure TSelectCards.HandleDeckPick(Sender: TObject);
begin
  with sender as TDeckButton do
  begin
    OnDeckChange(InternalState);
  end;
  ReadyButton.Text := 'Awaiting server...';
  ReadyButton.Enabled := false;
end;

procedure TSelectCards.HandleReady(Sender: TObject);
begin
  OnReady;
  ReadyButton.Text := 'Awaiting server...';
  ReadyButton.Enabled := false;
end;

procedure TSelectCards.HandleResize(Sender: TObject);
begin
  TitleLabel.Margins.Top := floor(ClientHeight / 25);
  TitleLabel.Font.Size := floor(ClientHeight / 40);

  ReadyButton.Height := floor(ClientHeight / 10);
  ReadyButton.Left := floor(ClientWidth / 2);
  ReadyButton.Width := ClientWidth - floor(ClientWidth / 8) - ReadyButton.Left;
  ReadyButton.Top := floor(ClientHeight - (ClientHeight / 10) - (ReadyButton.Height / 2));

  PostMessage(Handle, WM_USER +111, 0, 0);
end;

procedure TSelectCards.RenderState;
var
  tempDeck : TDeckButton;
  jsonObj : TJSONObject;
  s : string;
  I, index: Integer;
begin
  // Destroy the previous instance for uni-directional flow;
  Decks.Destroy;

  Decks := TScrollBox.Create(self);
  Decks.Visible := false;
  Decks.Parent := self;
  Decks.Top := 1;

  with Decks do
  begin
    BevelEdges := [];
    BorderStyle := bsNone;
    Parent := self;
    Color := rgb(238, 156, 139);
  end;

  Decks.Width := clientWidth;
  Decks.top := floor(ClientHeight / 25) + TitleLabel.Top + TitleLabel.Height;
  Decks.height := clientheight - floor(ClientHeight / 5) - decks.top;

  for I := 0 to State.Size - 1 do
  begin
    tempDeck := TDeckButton.Create(decks);
    tempDeck.Parent := decks;
    jsonObj := TJSONObject(State.Get(i));
    s := 'UNKNOWN - IS THE SERVER MODDED?';
    if jsonObj.Exists('name') then s := jsonObj.Get('name').JsonValue.Value;
    if jsonObj.Exists('index') then index := strtoint(jsonObj.Get('index').JsonValue.Value);

    with tempDeck do
    begin
      Height := floor(decks.clientheight / 5);
      top := i;
      align := alTop;
      AlignWithMargins:= true;
      margins.Top := ceil(decks.clientheight / 30);
      margins.Left := floor(decks.ClientWidth / 8);
      margins.Right := margins.left;
      text := s;
      InternalState := index;
      OnClick := HandleDeckPick;
    end;
  end;

  Decks.Visible := true;
end;

procedure TSelectCards.SetPushReady(const Value: TGenericEvent);
begin
  FPushReady := Value;
end;

procedure TSelectCards.SetSelected(const Value: boolean);
begin
  FSelected := Value;
  ReadyButton.Enabled := Value;
  if FSelected then ReadyButton.Text := 'Selected'
  else ReadyButton.Text := '';
end;

procedure TSelectCards.SetState(const Value: TJSONArray);
begin
  FState := Value;
  RenderState;
end;

procedure TSelectCards.HandleAfterResize(var Message: TMessage);
begin
  RenderState;
end;

{ TDeckButton }

procedure TDeckButton.SetInternalState(const Value: Variant);
begin
  FInternalState := Value;
end;

end.
