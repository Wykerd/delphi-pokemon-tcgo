unit preGameUI;

interface

uses
  Windows, ExtCtrls, Classes, StdCtrls, Controls, Graphics, Dialogs, DBXJSON,
  UIContainer, helpers, SysUtils, UIButton;

type
  TPreGameUI = class (TUIContainer)
  private
    FAuthenticated: boolean;
    FReady: boolean;
    FQueueState: string;
    procedure SetAuthenticated(const Value: boolean);
    procedure SetQueueState(const Value: string);
    procedure SetReady(const Value: boolean);
  published
    constructor Create(AOwner: TComponent); override;
    property Authenticated : boolean read FAuthenticated write SetAuthenticated;
    procedure AuthError(Reason : string);
    property QueueState : string read FQueueState write SetQueueState;
    property Ready: boolean read FReady write SetReady;
  end;

implementation

{ TPreGameUI }

procedure TPreGameUI.AuthError(Reason: string);
begin
  Caption := reason;
end;

constructor TPreGameUI.Create(AOwner: TComponent);
begin
  inherited;
  Caption := 'Authenticating with server...';
end;


procedure TPreGameUI.SetAuthenticated(const Value: boolean);
begin
  FAuthenticated := Value;
  if value then Caption := 'Authenticated';
end;

procedure TPreGameUI.SetQueueState(const Value: string);
begin
  FQueueState := Value;
end;

procedure TPreGameUI.SetReady(const Value: boolean);
begin
  FReady := Value;
end;

end.
