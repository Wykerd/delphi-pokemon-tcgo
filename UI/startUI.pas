unit startUI;

interface

uses
  UIContainer, Controls, Classes, ExtCtrls, UIButton, Dialogs, Types, Math;

type
  TStartUI = class (TUIContainer)
  private
    FBackground: TImage;
    ServersButton : TUIButton;
    FOnServersClick: TNotifyEvent;
    procedure SetBackground(const Value: TImage);
    procedure HandleResize (Sender : TObject);
    procedure SetOnServersClick(const Value: TNotifyEvent);
  published
    constructor Create (AOwner: TComponent); override;
    property Background : TImage read FBackground write SetBackground;
    property OnServersClick : TNotifyEvent read FOnServersClick write SetOnServersClick;
  end;

implementation

{ TStartUI }

constructor TStartUI.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Start UI';
  Background := TImage.Create(self);
  Background.Stretch := true;
  Background.Parent := self;
  Background.Picture.Bitmap.LoadFromResourceName(HInstance, 'BlankResource');

  ServersButton := TUIButton.Create(self);
  ServersButton.Parent := self;
  ServersButton.Text := 'Join A Server';
  ServersButton.Align := alTop;
  ServersButton.AlignWithMargins := true;
  ServersButton.Top := 100;
  ServersButton.Margins.Bottom := 20;

  OnResize := HandleResize;
end;

procedure TStartUI.HandleResize(Sender: TObject);
begin
  ServersButton.Height := floor(clientheight / 5);
  ServersButton.FontSize := floor(clientheight / 15);
  ServersButton.Margins.Top := floor((clientheight / 2) - ServersButton.Height);
  ServersButton.Margins.Left := floor(ClientWidth / 10);
  ServersButton.Margins.Right := floor(ClientWidth / 10);

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

end.
