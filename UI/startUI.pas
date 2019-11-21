unit startUI;

interface

// Color pallettes from
// http://pokepalettes.com
// This page uses charizard

uses
  UIContainer, Controls, Classes, ExtCtrls, UIButton, Dialogs, Types, Math,
  GIFImg, Graphics, UIImgButton;

type
  TStartUI = class (TUIContainer)
  private
    FBackground: TImage;
    ServersButton : TUIButton;
    SettingsButton : TUIImgButton;
    TradesButton : TUIButton;
    FOnServersClick: TNotifyEvent;
    FOnTradesClick: TNotifyEvent;
    procedure SetBackground(const Value: TImage);
    procedure HandleResize (Sender : TObject);
    procedure SetOnServersClick(const Value: TNotifyEvent);
    procedure SetOnTradesClick(const Value: TNotifyEvent);
  published
    constructor Create (AOwner: TComponent); override;
    property Background : TImage read FBackground write SetBackground;
    property OnServersClick : TNotifyEvent read FOnServersClick write SetOnServersClick;
    property OnTradesClick : TNotifyEvent read FOnTradesClick write SetOnTradesClick;
  end;

implementation

{ TStartUI }

constructor TStartUI.Create(AOwner: TComponent);
var
  rs : TResourceStream;
  gif : TGIFImage;
begin
  inherited;
  Caption := 'Start UI';
  Background := TImage.Create(self);
  Background.Stretch := true;
  Background.Parent := self;
  // Load the background image from resource
  gif := TGIFImage.Create;
  try
    RS := TResourceStream.Create(hInstance, 'StartBackground', RT_RCDATA);
    try
      gif.LoadFromStream(RS);
      gif.Animate := true;
      Background.Picture.Graphic := gif;
    finally
      RS.Free;
    end;
  finally
    gif.Free;
  end;

  ServersButton := TUIButton.Create(self);
  ServersButton.Parent := self;
  ServersButton.Text := 'Join A Server';
  ServersButton.Align := alTop;
  ServersButton.AlignWithMargins := true;
  ServersButton.Top := 100;
  ServersButton.Margins.Bottom := 0;

  TradesButton := TUIButton.Create(self);
  TradesButton.Parent := self;
  TradesButton.Text := 'Trade';
  TradesButton.Align := alTop;
  TradesButton.AlignWithMargins := true;
  TradesButton.Top := 150;
  TradesButton.TextOffset := 8;

  SettingsButton := TUIImgButton.Create(self);
  SettingsButton.IconName := 'SettingsIcon';
  SettingsButton.Parent := self;

  OnResize := HandleResize;
end;

procedure TStartUI.HandleResize(Sender: TObject);
begin
  ServersButton.Height := floor(clientheight / 5);
  ServersButton.Margins.Top := floor((clientheight / 2) - ServersButton.Height - (ServersButton.Height / 7));
  ServersButton.Margins.Left := floor(ClientWidth / 2.25);
  ServersButton.Margins.Right := floor(ClientWidth / 15);

  TradesButton.Height := floor(clientheight / 5);
  TradesButton.Margins.Top := floor(ServersButton.Height / 7);
  TradesButton.Margins.Right := floor((ClientWidth / 15) + (ServersButton.Height / 7) + TradesButton.Height);
  TradesButton.Margins.Left := floor(ClientWidth / 2.25);

  SettingsButton.Height := TradesButton.Height;
  SettingsButton.Width := SettingsButton.Height;
  SettingsButton.Top := TradesButton.Top;
  SettingsButton.Left := ServersButton.Left + ServersButton.Width - SettingsButton.Width;

  Background.Height := self.ClientHeight;
  Background.Width := self.ClientWidth;
end;

procedure TStartUI.SetBackground(const Value: TImage);
begin
  FBackground := Value;
end;

procedure TStartUI.SetOnServersClick(const Value: TNotifyEvent);
begin
  FOnServersClick := Value;
  ServersButton.OnClick := Value;
end;

procedure TStartUI.SetOnTradesClick(const Value: TNotifyEvent);
begin
  FOnTradesClick := Value;
  TradesButton.OnClick := Value;
end;

end.
