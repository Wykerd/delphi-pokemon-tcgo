unit startUI;

interface

uses
  UIContainer, Controls, Classes, ExtCtrls, UIButton, Dialogs, Types;

type
  TClientStart = procedure (Host : string; Port : integer) of object;

  TStartUI = class (TUIContainer)
  private
    FBackground: TImage;
    ServersButton : TUIButton;
    procedure SetBackground(const Value: TImage);
  published
    constructor Create (AOwner: TComponent); override;
    property Background : TImage read FBackground write SetBackground;
    procedure Render;
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
  Background.Align := alClient;
  Background.Picture.Bitmap.LoadFromResourceName(HInstance, 'BlankResource');

  ServersButton := TUIButton.Create(self);
  ServersButton.Parent := self;
  ServersButton.Text := 'Join A Server';
  ServersButton.Align := alTop;
end;

procedure TStartUI.Render;
begin
  ServersButton.OnMouseLeave(nil);
  Showmessage('ee');
end;

procedure TStartUI.SetBackground(const Value: TImage);
begin
  FBackground := Value;
end;

end.
