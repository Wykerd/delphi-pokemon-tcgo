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
    procedure HandleShowServers (Sender: TObject);
    procedure HandleShowStart (Sender: TObject);
    procedure HandleStartGame (Sender: TObject);
  published
    constructor CreateNew (AOwner : TComponent; Dummy: integer); override;
    property GameUI : TGameUI read FGame write SetGame;
    property StartUI : TStartUI read FStart write SetStart;
    property ServersUI : TServersUI read FServersUI write SetServersUI;
    property StartTrigger : TClientStart read FStartTrigger write SetStartTrigger;
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

  ServersUI := TServersUI.Create(self);
  ServersUI.Parent := self;
  ServersUI.Visible := false;
  ServersUI.OnBackClick := HandleShowStart;

  DoubleBuffered := true;
  Caption := 'Client';
  ClientHeight := 360;
  ClientWidth := 640;

  StartUI.OnServersClick := HandleShowServers;
  ServersUI.OnStartClient := HandleStartGame;
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
  GameUI.Init;
end;

procedure TClientUI.HandleShowServers(Sender: TObject);
begin
  StartUI.Visible := false;
  GameUI.Visible := false;
  ServersUI.Visible := true;
end;

procedure TClientUI.HandleShowStart(Sender: TObject);
begin
  StartUI.Visible := true;
  GameUI.Visible := false;
  ServersUI.Visible := false;
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
  ServersUI.StartTrigger := Value;
end;

end.
