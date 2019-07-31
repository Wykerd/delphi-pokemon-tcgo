unit clientUI;

interface
// Page 148
uses
  Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, gameUI, startUI, serversUI;

type
  TClientUI = class (TForm)
  private
    FGame: TGameUI;
    FStart: TStartUI;
    FStartTrigger: TClientStart;
    FServersUI: TServersUI;
    procedure SetGame(const Value: TGameUI);
    procedure SetStart(const Value: TStartUI);
    procedure SetStartTrigger(const Value: TClientStart);
    procedure SetServersUI(const Value: TServersUI);
  published
    constructor CreateNew (AOwner : TComponent; Dummy: integer); override;
    property GameUI : TGameUI read FGame write SetGame;
    property StartUI : TStartUI read FStart write SetStart;
    property ServersUI : TServersUI read FServersUI write SetServersUI;
    property StartTrigger : TClientStart read FStartTrigger write SetStartTrigger;
  end;

implementation

{ TClientUI }

constructor TClientUI.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  GameUI := TGameUI.Create(self);
  GameUI.Parent := self;
  GameUI.Visible := false;

  StartUI := TStartUI.Create(self);
  StartUI.Parent := self;

  DoubleBuffered := true;
  Caption := 'Client';
  ClientHeight := 360;
  ClientWidth := 640;
end;

procedure TClientUI.SetGame(const Value: TGameUI);
begin
  FGame := Value;
end;

procedure TClientUI.SetServersUI(const Value: TServersUI);
begin
  FServersUI := Value;
end;

procedure TClientUI.SetStart(const Value: TStartUI);
begin
  FStart := Value;
end;

procedure TClientUI.SetStartTrigger(const Value: TClientStart);
begin
  FStartTrigger := Value;
end;

end.
