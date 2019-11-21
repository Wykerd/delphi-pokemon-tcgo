// Copyright 2019 Daniel Wykerd
//
// TClientUI Class
//
// Author:      Daniel Wykerd
// Year:        2019
//
// Function:    Contains all the views of the game
//
// Description: The TClientUI is a decendant of the TForm Class and is used as
//              the main container for all the TUIContainer views.
unit clientUI;

interface

uses
  Windows, Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, DBXJSON,
  Controls, gameUI, startUI, serversUI, tradeUI, preGameUI, versions;

type
  TClientUI = class (TForm)
  private
    FGame: TGameUI;
    FStart: TStartUI;
    FStartTrigger: TClientStart;
    FServersUI: TServersUI;
    FTradeUI: TTradeUI;
    FPreGameUI: TPreGameUI;
    procedure SetGame(const Value: TGameUI);
    procedure SetStart(const Value: TStartUI);
    procedure SetStartTrigger(const Value: TClientStart);
    procedure SetServersUI(const Value: TServersUI);
    procedure HandleShowServers (Sender: TObject);
    procedure HandleShowStart (Sender: TObject);
    procedure HandleShowTrade (Sender: TObject);
    procedure HandleStartGame (Sender: TObject);
    procedure HandleStartServer(Sender: TObject);
    procedure SetTradeUI(const Value: TTradeUI);
    procedure SetPreGameUI(const Value: TPreGameUI);
  published
    constructor CreateNew (AOwner : TComponent; Dummy: integer); override;
    property GameUI : TGameUI read FGame write SetGame;
    property StartUI : TStartUI read FStart write SetStart;
    property PreGameUI : TPreGameUI read FPreGameUI write SetPreGameUI;
    property TradeUI : TTradeUI read FTradeUI write SetTradeUI;
    property ServersUI : TServersUI read FServersUI write SetServersUI;
    property StartTrigger : TClientStart read FStartTrigger write SetStartTrigger;
    procedure ShowGameUI;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{ TClientUI }

constructor TClientUI.CreateNew(AOwner: TComponent; Dummy: integer);
begin
  inherited;
  // Create all the application views as Panels as they are basically forms,
  // but can be swapped out without the user being directed to a new window/form
  // This, in my opinion, creates better flow in the User eXperience.
  GameUI := TGameUI.Create(self);
  GameUI.Parent := self;
  GameUI.Visible := false;

  StartUI := TStartUI.Create(self);
  StartUI.Parent := self;

  TradeUI := TTradeUI.Create(self);
  TradeUI.Parent := self;
  TradeUI.Visible := false;

  ServersUI := TServersUI.Create(self);
  ServersUI.Parent := self;
  ServersUI.Visible := false;
  ServersUI.OnBackClick := HandleShowStart;

  PreGameUI := TPreGameUI.Create(self);
  PreGameUI.Parent := self;
  PreGameUI.Visible := false;

  DoubleBuffered := true;
  Caption := 'Client - ' + VERSION_NAME;
  ClientHeight := 360;
  ClientWidth := 640;

  // Handle events
  StartUI.OnServersClick := HandleShowServers;
  StartUI.OnTradesClick := HandleShowTrade;
  ServersUI.OnStartClient := HandleStartServer;
  ServersUI.StartTrigger := StartTrigger;
end;

procedure TClientUI.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Make the client window appear on the taskbar
  // for BETTER UX
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := GetDesktopWindow;
end;

procedure TClientUI.HandleStartGame(Sender: TObject);
begin
  StartUI.Visible := false;
  GameUI.Visible := true;
  ServersUI.Visible := false;
  TradeUI.Visible := false;
  PreGameUI.Visible := false;
  GameUI.Init;
  GameUI.SetFocus;
end;

procedure TClientUI.HandleStartServer(Sender: TObject);
begin
  StartUI.Visible := false;
  GameUI.Visible := false;
  ServersUI.Visible := false;
  TradeUI.Visible := false;
  PreGameUI.Visible := true;
end;

procedure TClientUI.HandleShowServers(Sender: TObject);
begin
  StartUI.Visible := false;
  GameUI.Visible := false;
  ServersUI.Visible := true;
  TradeUI.Visible := false;
  PreGameUI.Visible := false;
end;

procedure TClientUI.HandleShowStart(Sender: TObject);
begin
  StartUI.Visible := true;
  GameUI.Visible := false;
  ServersUI.Visible := false;
  TradeUI.Visible := false;
  PreGameUI.Visible := false;
end;

procedure TClientUI.HandleShowTrade(Sender: TObject);
begin
  StartUI.Visible := false;
  GameUI.Visible := false;
  ServersUI.Visible := false;
  TradeUI.Visible := true;
  PreGameUI.Visible := false;
  TradeUI.Init;
end;

procedure TClientUI.SetGame(const Value: TGameUI);
begin
  FGame := Value;
end;

procedure TClientUI.SetPreGameUI(const Value: TPreGameUI);
begin
  FPreGameUI := Value;
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
  ServersUI.StartTrigger := Value;
end;

procedure TClientUI.SetTradeUI(const Value: TTradeUI);
begin
  FTradeUI := Value;
end;

procedure TClientUI.ShowGameUI;
begin
  HandleStartGame(self);
end;

end.
