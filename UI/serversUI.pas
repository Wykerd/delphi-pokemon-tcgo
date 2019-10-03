unit serversUI;

interface

uses
  Windows, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, Math,
  Forms, System.JSON, UIContainer, helpers, SysUtils, UIButton, UIImgButton;

type
  TClientStart = procedure (Host : string; Port : integer; ServerIndex: integer = -1) of object;

  TServerListing = class (TPanel)
  private
    FName: string;
    FLName: string;
    FPort: integer;
    FMotd: string;
    FHost: string;
    FArrayIndex: integer;
    procedure SetHost(const Value: string);
    procedure SetLName(const Value: string);
    procedure SetMotd(const Value: string);
    procedure SetName(const Value: string);
    procedure SetPort(const Value: integer);
    procedure HandleMouseEnter (Sender: TObject);
    procedure HandleMouseLeave (Sender: TObject);
    procedure SetArrayIndex(const Value: integer);
  published
    constructor CreateWithState (AOwner : TComponent; sName, sLName,
      sMotd, sHost : string; iPort : integer);
    procedure Paint; override;
    property Name: string read FName write SetName;
    property Host: string read FHost write SetHost;
    property Port: integer read FPort write SetPort;
    property LName: string read FLName write SetLName;
    property Motd: string read FMotd write SetMotd;
    property ArrayIndex: integer read FArrayIndex write SetArrayIndex;
  end;

  TAddServerUI = class (TUIContainer)
  private
    FPath: string;
    AddServerBtn : TUIButton;
    BackButton : TUIImgButton;
    CenterPanel : TPanel;
    procedure SetPath(const Value: string);
    procedure HandleResize (Sender: TObject);
    procedure HandleAddClick (Sender: TObject);
    procedure HandleBackClick (Sender: TObject);
  published
    constructor Create (AOwner : TComponent); override;
    property Path : string read FPath write SetPath;
  end;

  TServersUI = class (TUIContainer)
  private
    FServers: TJSONArray;
    Listings : array of TServerListing;
    FStartTrigger: TClientStart;
    FOnStartClient: TNotifyEvent;

    // Buttons
    AddServer : TUIButton;
    BackButton : TUIImgButton;
    DirectButton : TUIButton;
    FServersPath: string;
    //

    // Dialogs
    AddServerUI : TAddServerUI;
    FOnBackClick: TNotifyEvent;
    //

    procedure SetServers(const Value: TJSONArray);
    procedure HandleResize (Sender : TObject);
    procedure SetStartTrigger(const Value: TClientStart);
    procedure HandleServerClick (Sender : TObject);
    procedure SetOnStartClient(const Value: TNotifyEvent);
    procedure SetServersPath(const Value: string);
    procedure HandleAddClick (Sender: TObject);
    procedure SetOnBackClick(const Value: TNotifyEvent);
  published
    constructor Create (AOwner: TComponent); override;
    procedure LoadFromFile(s : string);
    property Servers : TJSONArray read FServers write SetServers;
    property StartTrigger : TClientStart read FStartTrigger write SetStartTrigger;
    property OnStartClient : TNotifyEvent read FOnStartClient write SetOnStartClient;
    property OnBackClick : TNotifyEvent read FOnBackClick write SetOnBackClick;
    property ServersPath : string read FServersPath write SetServersPath;
  public
    ListingsContainer : TScrollbox;
  end;

implementation

{ TServersUI }

constructor TServersUI.Create(AOwner: TComponent);
begin
  inherited;

  ListingsContainer := TScrollBox.Create(self);

  with ListingsContainer do
  begin
    Align := alClient;
    AlignWithMargins := true;
    Margins.Left := 0;
    Margins.Right := 0;
    BevelEdges := [];
    BorderStyle := bsNone;
    Parent := self;
    Color := rgb(238, 180, 90);
  end;

  Color := rgb(24, 139, 180);

  AddServer := TUIButton.Create(self);
  AddServer.Parent := self;
  AddServer.Text := 'Add Server';
  AddServer.OnClick := HandleAddClick;

  DirectButton := TUIButton.Create(self);
  DirectButton.Parent := self;
  DirectButton.Text := 'Direct Connect';

  AddServerUI := TAddServerUI.Create(self);
  AddServerUI.Parent := self;
  AddServerUI.Visible := false;

  BackButton := TUIImgButton.Create(self);
  BackButton.Parent := self;
  BackButton.IconName := 'BackIcon';

  OnResize := HandleResize;
end;

procedure TServersUI.HandleAddClick(Sender: TObject);
begin
  AddServerUI.Visible := true;
  AddServerUI.BringToFront;
end;

procedure TServersUI.HandleResize(Sender: TObject);
var
  i : integer;
begin
  for I := 0 to Length(Listings) - 1 do
  begin
    with Listings[i] do
    begin
      Margins.Top := ceil(self.clientheight / 30);
      Margins.Left := floor(self.ClientWidth / 8);
      Margins.Right := floor(self.ClientWidth / 8);
      Height := floor(self.clientheight / 8);
    end;
  end;

  ListingsContainer.Margins.Top := floor(ClientHeight / 10);
  ListingsContainer.Margins.Bottom := floor(ClientHeight / 5);

  BackButton.Height := floor(ClientHeight / 10);
  BackButton.Top := floor(ClientHeight - (ClientHeight / 10) - (BackButton.Height / 2));
  BackButton.Left := floor(ClientWidth / 8);
  BackButton.Width := BackButton.Height;

  AddServer.Height := floor(ClientHeight / 10);
  AddServer.Top := floor(ClientHeight - (ClientHeight / 10) - (AddServer.Height / 2));
  AddServer.Left := BackButton.Left + BackButton.Width + floor(BackButton.Height / 7);
  AddServer.Width := floor((ClientWidth / 2) - (ClientWidth / 8) - (AddServer.Height / 7) - (BackButton.Width / 2));

  DirectButton.Left := AddServer.Width + AddServer.Left + floor(AddServer.Height / 7);
  DirectButton.Height := floor(ClientHeight / 10);
  DirectButton.Top := floor(ClientHeight - (ClientHeight / 10) - (DirectButton.Height / 2));
  DirectButton.Width := AddServer.Width;
end;

procedure TServersUI.HandleServerClick(Sender: TObject);
var
  listing : TServerListing;
begin
  listing := TServerListing(Sender);
  StartTrigger(listing.Host, listing.Port, listing.ArrayIndex);
  OnStartClient(self);
end;

procedure TServersUI.LoadFromFile(s: string);
var
  tF : TextFile;
  tmp, dat : string;
  JSON : TJSONObject;
  JSONArr : TJSONArray;
begin
  ServersPath := s;

  AssignFile(tF, s);

  try
    try
      reset(tF);
    except
      raise Exception.CreateFmt('Could not load server list at %s', [QuotedStr(s)]);
    end;
  finally
    while not eof(tF) do
    begin
      Readln (tF, tmp);
      dat := dat + tmp;
    end;

    JSON := TJSONObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(dat)),0));

    if JSON = nil then
      raise Exception.CreateFmt('Invalid JSON for server list at %s', [QuotedStr(s)]);

    if JSON.Get('servers') = nil then
      raise Exception.CreateFmt('Invalid server list at %s', [QuotedStr(s)]);

    JSONArr := TJSONArray(JSON.Get('servers').JsonValue);

    if JSONArr = nil then
      raise Exception.CreateFmt('Invalid JSON array in server list at %s', [QuotedStr(s)]);

    Servers := JSONArr;
  end;

  closefile(tF);
end;

procedure TServersUI.SetOnBackClick(const Value: TNotifyEvent);
begin
  FOnBackClick := Value;
  BackButton.OnClick := Value;
end;

procedure TServersUI.SetOnStartClient(const Value: TNotifyEvent);
var
  i : integer;
begin
  FOnStartClient := Value;
end;

procedure TServersUI.SetServers(const Value: TJSONArray);
var
  I: Integer;
  JSONTmp, JSONListingTmp : TJSONObject;
begin
  FServers := Value;

  // Destroy the previous instances if there are any
  for I := 0 to Length(Listings) - 1 do
  begin
    Listings[i].Destroy;
  end;

  SetLength(Listings, Servers.Size);

  for I := 0 to Servers.Size - 1 do
  begin
    JSONTmp := TJSONObject(Servers.Get(i));

    if (JSONTmp.Get('host') = nil) or (JSONTmp.Get('port') = nil) or
      (JSONTmp.Get('name') = nil) or (JSONTmp.Get('listing') = nil) then
      exit;

    JSONListingTmp := TJSONObject(JSONTmp.Get('listing').JsonValue);

    if (JSONListingTmp.Get('motd') = nil) or (JSONListingTmp.Get('name') = nil) then
      exit;

     Listings[i] := TServerListing.CreateWithState(
      ListingsContainer,
      JSONTmp.Get('name').JsonValue.Value,
      JSONListingTmp.Get('name').JsonValue.Value,
      JSONListingTmp.Get('motd').JsonValue.Value,
      JSONTmp.Get('host').JsonValue.Value,
      strtoint(JSONTmp.Get('port').JsonValue.Value));

    with Listings[i] do
    begin
      Parent := ListingsContainer;
      Top := i * 10;
      OnClick := HandleServerClick;
      ArrayIndex := i;
    end;
  end;
end;

procedure TServersUI.SetServersPath(const Value: string);
begin
  FServersPath := Value;
end;

procedure TServersUI.SetStartTrigger(const Value: TClientStart);
begin
  FStartTrigger := Value;
end;

{ TServerListing }

constructor TServerListing.CreateWithState(AOwner: TComponent; sName, sLName,
  sMotd, sHost: string; iPort: integer);
begin
  inherited Create (AOwner);

  AlignWithMargins := true;

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Lucida Console';

  Align := alTop;

  Name := sName;
  LName := sLName;
  Motd := sMotd;
  Host := sHost;
  Port := iPort;

  OnMouseEnter := HandleMouseEnter;
  OnMouseLeave := HandleMouseLeave;
end;

procedure TServerListing.HandleMouseEnter(Sender: TObject);
var
  Bitmap : TBitmap;
begin
  inherited;

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'BlankResource');
    Canvas.StretchDraw(Self.ClientRect, Bitmap);
    Canvas.Font.Color := rgb(222, 77, 64);
    Canvas.Font.Size := floor(height / 5);
    Canvas.TextOut(floor(width / 10), floor((height / 2) - canvas.Font.Size - 5), Name + ' - ' + LName);
    Canvas.Font.Color := rgb(84, 84, 84);
    Canvas.Font.Size := floor(height / 5);
    Canvas.TextOut(floor(width / 10), floor((height / 2) + 5), Motd);
  finally
    Bitmap.Free;
  end;
end;

procedure TServerListing.HandleMouseLeave(Sender: TObject);
begin
  Paint;
end;

procedure TServerListing.Paint;
var
  Bitmap : TBitmap;
begin
  inherited;

  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'BlankResource');
    Canvas.StretchDraw(Self.ClientRect, Bitmap);
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size := floor(height / 5);
    Canvas.TextOut(floor(width / 10), floor((height / 2) - canvas.Font.Size - 5), Name + ' - ' + LName);
    Canvas.Font.Color := rgb(84, 84, 84);
    Canvas.Font.Size := floor(height / 5);
    Canvas.TextOut(floor(width / 10), floor((height / 2) + 5), Motd);
  finally
    Bitmap.Free;
  end;
end;

procedure TServerListing.SetArrayIndex(const Value: integer);
begin
  FArrayIndex := Value;
end;

procedure TServerListing.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TServerListing.SetLName(const Value: string);
begin
  FLName := Value;
end;

procedure TServerListing.SetMotd(const Value: string);
begin
  FMotd := Value;
end;

procedure TServerListing.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TServerListing.SetPort(const Value: integer);
begin
  FPort := Value;
end;

{ TAddServerUI }

constructor TAddServerUI.Create(AOwner: TComponent);
begin
  inherited;

  CenterPanel := TPanel.Create(self);

  with CenterPanel do
  begin
    Align := alClient;
    AlignWithMargins := true;
    Margins.Left := 0;
    Margins.Right := 0;
    BorderStyle := bsNone;
    BevelEdges := [];
    BevelOuter := bvNone;
    Parent := self;
    Color := rgb(238, 180, 90);
    ParentBackground := false;
  end;

  Color := rgb(24, 139, 180);

  AddServerBtn := TUIButton.Create(self);
  AddServerBtn.Parent := self;
  AddServerBtn.Text := 'Add Server';
  AddServerBtn.OnClick := HandleAddClick;

  BackButton := TUIImgButton.Create(self);
  BackButton.Parent := self;
  BackButton.IconName := 'BackIcon';
  BackButton.OnClick := Handlebackclick;

  OnResize := HandleResize;
end;

procedure TAddServerUI.HandleAddClick(Sender: TObject);
begin

end;

procedure TAddServerUI.HandleBackClick(Sender: TObject);
begin
  Visible := false;
end;

procedure TAddServerUI.HandleResize(Sender: TObject);
begin
  CenterPanel.Margins.Top := floor(ClientHeight / 10);
  CenterPanel.Margins.Bottom := floor(ClientHeight / 5);

  BackButton.Height := floor(ClientHeight / 10);
  BackButton.Top := floor(ClientHeight - (ClientHeight / 10) - (BackButton.Height / 2));
  BackButton.Left := floor(ClientWidth / 8);
  BackButton.Width := BackButton.Height;

  AddServerBtn.Height := floor(ClientHeight / 10);
  AddServerBtn.Top := floor(ClientHeight - (ClientHeight / 10) - (AddServerBtn.Height / 2));
  AddServerBtn.Left := BackButton.Left + BackButton.Width + floor(BackButton.Height / 7);
  AddServerBtn.Width :=
    floor(ClientWidth - (ClientWidth / 4) - (AddServerBtn.Height / 7) - BackButton.Width);
end;

procedure TAddServerUI.SetPath(const Value: string);
begin
  FPath := Value;
end;

end.