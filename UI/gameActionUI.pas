// Copyright 2019 Daniel Wykerd
//
// gameActionUI Unit
//
// Author:      Daniel Wykerd
// Year:        2019
//
// Function:    Handles most of the client side logic.
//
// Description: Contains the UIContainers for interacting with the game, the
//              server and changing the state of the game.

unit gameActionUI;

interface

uses
  Windows, Classes, Forms, Dialogs, StdCtrls, Graphics, SysUtils, helpers, System.JSON,
  Controls, ExtCtrls, UIContainer, OpenGL, Textures, pkmCard, clientState, UIButton,
  math, strutils;

type
  TActionCardSprite = class (TPanel)
  published
    constructor Create(AOwner: TComponent);
    procedure Paint; override;
  public
    sprite : TBitmap;
  end;

  TModelViewerContext = class (TObject)
    constructor CreateWithState(_action: TJSONObject);
  public
    action: TJSONObject;
  end;

  // The base class for all the Action views. Displays the card modal and provides
  // the container for the buttons as components_view.
  //
  // Handles the container scaling so inherited classes require less code.
  //
  // It also parses the gameplay state.
  TModelViewer = class (TUIContainer)
  private
    model : TCardModel;
    stage: string;
    callback : TProc<TModelViewerContext>;
    card_view: TActionCardSprite;
    component_view : TPanel;
    btnCancel: TUIButton;
    retreat, energy_attach: boolean;
    procedure HandleCancel(sender: TObject);
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); virtual;
    procedure HandleResize(Sender: TObject); virtual;
    destructor Destroy; override;
  end;

  THandPokemonMove = (hpmActive, hpmBench);

  THandUI = class (TModelViewer)
  private
    btnAction1, btnAction2 : TUIButton;
    move_to: THandPokemonMove;
    state: TJSONObject;
    has_state_passed: boolean;
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); override;
    procedure ShowWithFullState(_model: TCardModel; _stage: string;
      _state: TJSONObject; _callback: TProc<TModelViewerContext>);
    procedure HandleResize(Sender: TObject); override;
    // returns the indexes!
    // requires FRenderCache and
    procedure FindStageInBenched(_stage: string; callback: TProc<TArray<integer>>);
    destructor Destroy; override;
  end;

  TActiveUI = class (TModelViewer)
  private
    btnAttack1, btnAttack2, btnRetreat : TUIButton;
    attached_energy: TArray<string>;
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); override;
    destructor Destroy; override;
    function HasRequiredEnergy(req_energy: TJSONArray): boolean;
    procedure HandleResize(Sender: TObject); override;
  end;

// Used to scale the card.
const
  CARD_ASPECT_RATIO = 420/590 ;

implementation

{ TActiveUI }

constructor TActiveUI.Create(AOwner: TComponent);
begin
  inherited;
  btnAttack1 := TUIButton.Create(component_view);
  btnAttack1.Parent := component_view;
  btnAttack1.Text := 'Use Attack One';

  btnAttack2 := TUIButton.Create(component_view);
  btnAttack2.Parent := component_view;
  btnAttack2.Text := 'Use Attack Two';

  btnRetreat := TUIButton.Create(component_view);
  btnRetreat.Parent := component_view;
  btnRetreat.Text := 'Retreat Card';
end;

destructor TActiveUI.Destroy;
begin
  freeandnil(btnAttack1);
  freeandnil(btnAttack2);
  freeandnil(btnRetreat);
  inherited;
end;

procedure TActiveUI.HandleResize(Sender: TObject);
begin
  inherited;

  with btnAttack1 do
  begin
    align := altop;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := 0;
  end;

  with btnAttack2 do
  begin
    align := altop;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := 0;
  end;

  with btnRetreat do
  begin
    align := alBottom;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := 0;
  end;

end;

function TActiveUI.HasRequiredEnergy(req_energy: TJSONArray): boolean;
var
  _attached_energy: TArray<string>;
  I: Integer;
  J: Integer;
  b: boolean;
const
  FOUND = '_';
begin
  result := true;

  setlength(_attached_energy, length(attached_energy));

  for I := Low(attached_energy) to High(attached_energy) do
    _attached_energy[i] := attached_energy[i];

  for I := 0 to req_energy.Count do
  begin
    b := false;
    for J := 0 to length(_attached_energy) - 1 do
    begin
      if _attached_energy[j] = req_energy.Get(i).Value then
      begin
        _attached_energy[j] := FOUND;
        b := true;
        break;
      end;
    end;
    if not b then exit(false);
  end;
end;

procedure TActiveUI.ShowWithState(_model: TCardModel; _stage: string;
  _callback: TProc<TModelViewerContext>);
begin
  inherited;
  btnAttack1.Visible := false;
  btnAttack2.Visible := false;
  btnRetreat.Visible := false;

  if stage = 'init' then exit;


  model.Sprite.Data.ExistCall('data', procedure (j: TJSONPair)
  begin
    TJSONObject(j.JsonValue).ExistCall('attack1', procedure (p: TJSONPair)
    begin
      TJSONObject(p.JsonValue).ExistCall('energy', procedure (o: TJSONPair)
      begin
        if HasRequiredEnergy(TJSONArray(o.JsonValue)) then
          btnAttack1.Visible := true;
      end);
    end);

    TJSONObject(j.JsonValue).ExistCall('attack2', procedure (p: TJSONPair)
    begin
      TJSONObject(p.JsonValue).ExistCall('energy', procedure (o: TJSONPair)
      begin
        if HasRequiredEnergy(TJSONArray(o.JsonValue)) then
          btnAttack2.Visible := true;
      end);
    end);

    if retreat then
      TJSONObject(j.JsonValue).ExistCall('retreat-cost', procedure (c: TJSONPair)
      var
        retreat_cost: integer;
      begin
        try
          retreat_cost := strtoint(c.JsonValue.Value);
          model.Sprite.Data.ExistCall('attached-energy', procedure (js: TJSONPair)
          begin
            if TJSONArray(js.JsonValue).Count >= retreat_cost then
            begin
              btnRetreat.Visible := true;
            end;
          end);
        finally
          //
        end;
      end);
  end);
end;

{ TActionCardSprite }

constructor TActionCardSprite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelEdges := [];
  BevelOuter := bvNone;
  ParentBackground := false;
end;

procedure TActionCardSprite.Paint;
begin
  inherited;
  if sprite <> nil then
    Canvas.StretchDraw(Rect(0, 0, width, height), sprite);
end;

{ TModelViewer }

constructor TModelViewer.Create(AOwner: TComponent);
begin
  inherited;

  card_view := TActionCardSprite.Create(self);
  card_view.Parent := self;
  card_view.Color := clRed;

  color := rgb(148, 180, 197);;

  component_view := TPanel.Create(self);
  component_view.Parent := self;
  component_view.Color := rgb(238, 156, 139);
  with component_view do
  begin
    BevelEdges := [];
    BevelOuter := bvNone;
    ParentBackground := false;
  end;

  btnCancel := TUIButton.Create(component_view);
  btnCancel.Parent := component_view;
  btnCancel.Text := 'Cancel';
  btnCancel.OnClick := HandleCancel;

  onresize := handleresize;

  callback := procedure (context : TModelViewerContext)
  begin
    // Blank default procedure
  end;
end;

destructor TModelViewer.Destroy;
begin
  freeandnil(card_view);
  freeandnil(btnCancel);
  inherited;
end;

procedure TModelViewer.HandleCancel(sender: TObject);
begin
  callback(TModelViewerContext.CreateWithState(nil));
end;

procedure TModelViewer.HandleResize(Sender: TObject);
begin
  with card_view do
  begin
    align := alLeft;
    AlignWithMargins := true;
    Margins.Left := floor(self.ClientWidth / 16);
    Margins.Right := 0;
    Margins.Top := floor(self.ClientHeight / 16);
    Margins.Bottom := floor(self.ClientHeight / 16);
    width := round((self.ClientHeight - (2 * floor(self.ClientHeight / 16))) * CARD_ASPECT_RATIO);
    invalidate;
  end;

  with component_view do
  begin
    align := alClient;
    AlignWithMargins := true;
    Margins.Left := 0;
    Margins.Right := floor(self.ClientWidth / 16);
    Margins.Top := floor(self.ClientHeight / 16);
    Margins.Bottom := floor(self.ClientHeight / 16);
  end;

  with btnCancel do
  begin
    align := alBottom;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := floor(self.ClientHeight / 24);
  end;
end;

procedure TModelViewer.ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>);
var
  arr: TArray<string>;
begin
  model := _model;
  stage := _stage;
  callback := _callback;
  card_view.sprite := model.Sprite;
  card_view.Invalidate;

  retreat := false;
  energy_attach := false;

  // Check stage
  arr := strSplit(stage, '_');
  if length(arr) > 1 then
  begin
    if arr[0] = 'card' then
    begin
      stage := 'card';
      arr := strSplit(arr[1], '+');
      if MatchStr('retreat', arr) then retreat := true;
      if MatchStr('energy', arr) then energy_attach := true;
    end;
  end;
end;

{ TModelViewerContext }

constructor TModelViewerContext.CreateWithState(_action: TJSONObject);
begin
  inherited Create;
  action := _action;
end;

{ THandUI }

constructor THandUI.Create(AOwner: TComponent);
begin
  inherited;
  has_state_passed := false;

  btnAction1 := TUIButton.Create(component_view);
  btnAction1.Parent := component_view;

  btnAction2 := TUIButton.Create(component_view);
  btnAction2.Parent := component_view;
end;

destructor THandUI.Destroy;
begin

  inherited;
end;

procedure THandUI.FindStageInBenched(_stage: string;
  callback: TProc<TArray<integer>>);
begin
  //
end;

procedure THandUI.HandleResize(Sender: TObject);
begin
  inherited;
  with btnAction1 do
  begin
    align := altop;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := 0;
  end;

  with btnAction1 do
  begin
    align := altop;
    AlignWithMargins := true;

    Height := floor(self.ClientHeight / 10);

    Margins.Left := floor(self.ClientWidth / 24);
    Margins.Right := floor(self.ClientWidth / 24);
    Margins.Top := floor(self.ClientHeight / 24);
    Margins.Bottom := 0;
  end;
end;

procedure THandUI.ShowWithFullState(_model: TCardModel; _stage: string;
  _state: TJSONObject; _callback: TProc<TModelViewerContext>);
begin
  inherited ShowWithState(_model, _stage, _callback);
  if _state <> nil then has_state_passed := true else has_state_passed := false;

  // if pokemon
  // action1 -> if init -- move to active; else move to bench

  // if trainer
  // action1 -> use (in right stage)

  // if energy
  // btnAction1 -> Attach to active (if active present)
  btnAction1.Visible := false;
  btnAction2.Visible := false;

  model.Sprite.Data.ExistCall('type', procedure (j: TJSONPair)
  begin
    case Upcase(j.JsonValue.Value[1]) of
      'P':
        begin
          // Pokemon
          if stage = 'init' then
          begin
            model.Sprite.Data.ExistCall('data', procedure(dat: TJSONPair)
            begin
              TJSONObject(dat.JsonValue).ExistCall('stage', procedure (pp: TJSONPair)
              begin
                if pp.JsonValue.Value = '0' then
                begin
                  move_to := hpmActive;
                  btnAction1.Text := 'Move to active';
                  btnAction1.Visible := true;
                end;
              end);
            end);
          end
          else
          begin
            if has_state_passed then
              state.ExistCall('player', procedure (player: TJSONPair)
              begin
                model.Sprite.Data.ExistCall('data', procedure(dat: TJSONPair)
                begin
                  TJSONObject(dat.JsonValue).ExistCall('stage', procedure (pp: TJSONPair)
                  begin
                    if pp.JsonValue.Value <> '0' then
                    TJSONObject(player.JsonValue).ExistCall('active-card', procedure (active: TJSONPair)
                    begin
                      TJSONObject(active.JsonValue).ExistCall('id', procedure (active_id: TJSONPair)
                      begin
                        TJSONObject(dat.JsonValue).ExistCall('base', procedure (base: TJSONPair)
                        begin
                          if base.JsonValue.Value = active_id.JsonValue.Value then
                          begin
                            move_to := hpmBench;
                            btnAction2.Text := 'Attach to active';
                            btnAction2.Visible := true;
                          end;
                        end);
                      end);
                    end);

                    // Check the benched-cards
                    TJSONObject(player.JsonValue).ExistCall('benched-cards', procedure(benched: TJSONPair)
                    begin
                      if TJSONArray(benched.JsonValue).Count < 5 then
                      begin
                        if pp.JsonValue.Value = '0' then
                        begin
                          move_to := hpmBench;
                          btnAction1.Text := 'Move to bench';
                          btnAction1.Visible := true;
                        end
                        else
                        begin
                          // Handle non basic pokemon
                          if not (stage = 'init-bench') then
                          TJSONObject(dat.JsonValue).ExistCall('base', procedure (base: TJSONPair)
                          begin
                            FindStageInBenched(base.JsonValue.Value, procedure (indexes: TArray<integer>)
                            begin
                              if length(indexes) > 0 then
                              begin
                                move_to := hpmBench;
                                btnAction2.Text := 'Attach to bench';
                                btnAction2.Visible := true;
                              end;
                            end);
                          end);
                        end;
                      end;
                    end);
                  end);
                end);
              end);
          end;
        end;
      'E':
        begin
          if stage = 'card' then
          begin
            if energy_attach then
            begin
              btnAction1.Text := 'Attach to active';
              btnAction1.Visible := true;
              btnAction2.Text := 'Attach to benched';
              btnAction2.Visible := true;
            end;
          end;
        end;
      'T':
        begin
          if stage = 'card' then
          begin
            btnAction1.Text := 'Use';
            btnAction1.Visible := true;
          end;
        end;
    end;
  end);
end;

procedure THandUI.ShowWithState(_model: TCardModel; _stage: string;
  _callback: TProc<TModelViewerContext>);
begin
  inherited;
  ShowWithFullState(_model, _stage, nil, _callback);
end;

end.