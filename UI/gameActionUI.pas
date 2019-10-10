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
  math, strutils, variants;

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
    index: integer;
    card_container: TScrollBox;
    BenchCandidates: TArray<integer>;
    // handlers
    procedure HandleToActive (Sender: TObject);
    procedure handletobench_init (Sender: TObject);
    procedure HandleAttachActive (Sender: TObject);
    procedure HandleMoveBench (Sender: TObject);
    procedure HandleAttachBench (Sender: TObject);
    procedure HandleBenchAttachment (Sender: TObject);
    procedure HandleEnergyActive (Sender: TObject);
    procedure HandleEnergyBenched (Sender: TObject);
    procedure HandleBenchEnergyAttachment (Sender: TObject);
    procedure ShowCardContainer;
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); override;
    procedure ShowWithFullState(_model: TCardModel; _stage: string;
      _state: TJSONObject; _index: integer; _callback: TProc<TModelViewerContext>);
    procedure HandleResize(Sender: TObject); override;
    // returns the indexes!
    // requires FRenderCache and
    procedure FindStageInBenched(_stage: string; callback: TProc<TArray<integer>>);
    destructor Destroy; override;
  public
    RenderCache: TArray<TCardModel>;
    Benched: TArray<integer>;
  end;

  TActiveUI = class (TModelViewer)
  private
    btnAttack1, btnAttack2, btnRetreat, btnEnd : TUIButton;
    procedure HandleUseAttackOne (Sender: TObject);
    procedure HandleUseAttackTwo (Sender: TObject);
    procedure HandleRetreat (Sender: TObject);
    procedure HandleEndTurn (Sender: TObject);
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); override;
    destructor Destroy; override;
    function HasRequiredEnergy(req_energy: TJSONArray): boolean;
    procedure HandleResize(Sender: TObject); override;
  public
    Benched: TArray<integer>;
  end;

  TBenchUI = class (TModelViewer)
  private
    btnToActive : TUIButton;
    procedure HandleToActive (Sender: TObject);
  published
    constructor Create(AOwner: TComponent); override;
    procedure ShowWithState(_model: TCardModel; _stage: string; _callback: TProc<TModelViewerContext>); override;
    destructor Destroy; override;
    procedure HandleResize(Sender: TObject); override;
  public
    index: integer;
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
  btnAttack1.OnClick := HandleUseAttackOne;

  btnAttack2 := TUIButton.Create(component_view);
  btnAttack2.Parent := component_view;
  btnAttack2.Text := 'Use Attack Two';
  btnAttack2.OnClick := HandleUseAttackTwo;

  btnEnd := TUIButton.Create(component_view);
  btnEnd.Parent := component_view;
  btnEnd.Text := 'End your turn';
  btnEnd.OnClick := HandleEndTurn;

  btnRetreat := TUIButton.Create(component_view);
  btnRetreat.Parent := component_view;
  btnRetreat.Text := 'Retreat Card';
  btnRetreat.OnClick := HandleRetreat;
end;

destructor TActiveUI.Destroy;
begin
  freeandnil(btnAttack1);
  freeandnil(btnAttack2);
  freeandnil(btnRetreat);
  freeandnil(btnEnd);
  inherited;
end;

procedure TActiveUI.HandleEndTurn(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'end-turn'));

  // literally anything as data, as the server requires it!
  Data.AddPair(TJSONPair.Create('end', 'true'));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
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

  with btnEnd do
  begin
    align := alBottom;
    AlignWithMargins := true;

    visible := not ((stage = 'init') or (stage = 'set-active'));

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

procedure TActiveUI.HandleRetreat(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'retreat'));

  // literally anything as data, as the server requires it!
  Data.AddPair(TJSONPair.Create('retreat', 'true'));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure TActiveUI.HandleUseAttackOne(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'use-attack'));

  Data.AddPair(TJSONPair.Create('index', '1'));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure TActiveUI.HandleUseAttackTwo(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'use-attack'));

  Data.AddPair(TJSONPair.Create('index', '2'));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

function TActiveUI.HasRequiredEnergy(req_energy: TJSONArray): boolean;
var
  I: Integer;
  J: Integer;
  b: boolean;
  attached_energy: TArray<string>;
  arr: TJSONArray;
const
  FOUND = '_';
begin
  if model.Sprite.Data.Exists('attached-energy') then
  begin
    arr := TJSONArray(model.Sprite.Data.Get('attached-energy').JsonValue);
    SetLength(attached_energy, arr.Count);

    for I := Low(attached_energy) to High(attached_energy) do
      attached_energy[i] := arr.get(i).Value;

    for I := 0 to req_energy.Count - 1 do
    begin
      b := false;
      for J := 0 to length(attached_energy) - 1 do
      begin
        if attached_energy[j] = req_energy.Get(i).Value then
        begin
          attached_energy[j] := FOUND;
          b := true;
          break;
        end;
      end;
      if not b then exit(false);
    end;
  end
  else exit(false);
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
              if length(benched) > 0 then btnRetreat.Visible := true;
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
  freeandnil(btnAction1);
  FreeAndNil(btnAction2);
  inherited;
end;

procedure THandUI.FindStageInBenched(_stage: string;
  callback: TProc<TArray<integer>>);
var
  arr: TArray<integer>;
  i : integer;
begin
  setlength(arr, 0);
  for I := 0 to length(Benched) - 1 do
  begin
    if RenderCache[Benched[i]].Sprite.Data.Exists('type') then
    begin
      if RenderCache[Benched[i]].Sprite.Data.Get('type').JsonValue.Value = 'pokemon' then
      begin
        if RenderCache[Benched[i]].Sprite.Data.Exists('data') then
        begin
          if TJSONObject(RenderCache[Benched[i]].Sprite.Data.Get('data')).Exists('stage') then
          begin
            if TJSONObject(RenderCache[Benched[i]].Sprite.Data.Get('data')).Get('stage').JsonValue.Value = _stage then
            begin
              setlength(arr, length(arr) + 1);
              arr[high(arr)] := i;
            end;
          end;
        end;
      end;
    end;
  end;
  callback(arr);
end;

procedure THandUI.HandleAttachActive(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'evolve'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure THandUI.HandleAttachBench(Sender: TObject);
var
  I: Integer;
  _temp : TUIButtonStatefull;
begin
  showcardcontainer;
  for I := 0 to length(BenchCandidates) - 1 do
  begin
    _temp := TUIButtonStatefull.Create(card_container);
    _temp.Parent := card_container;
    _temp.internal_state := BenchCandidates[i].ToString;
    _temp.Text := RenderCache[benched[BenchCandidates[i]]].Sprite.Data.getvalue<string>('name');
    _temp.OnClick := HandleBenchAttachment;
    with _temp do
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
end;

procedure THandUI.HandleBenchAttachment(Sender: TObject);
var
  _sender: TUIButtonStatefull;
  Action, Data: TJSONObject;
begin
  if sender is TUIButtonStatefull then
  begin
    _sender := sender as TUIButtonStatefull;
    Action := TJSONObject.Create;
    Data := TJSONObject.Create;

    Action.AddPair(TJSONPair.Create('action', 'game-action'));

    Action.AddPair(TJSONPair.Create('game-action', 'evolve-bench'));

    Data.AddPair(TJSONPair.Create('index', index.ToString));

    Data.AddPair(TJSONPair.Create('bench-index', VarToStr(_sender.internal_state)));

    Action.AddPair(TJSONPair.Create('data', Data));

    card_container.Visible := false;

    callback(TModelViewerContext.CreateWithState(Action));
  end;
  card_container.Visible := false;
end;

procedure THandUI.HandleBenchEnergyAttachment(Sender: TObject);
var
  _sender: TUIButtonStatefull;
  Action, Data: TJSONObject;
begin
  if sender is TUIButtonStatefull then
  begin
    _sender := sender as TUIButtonStatefull;
    Action := TJSONObject.Create;
    Data := TJSONObject.Create;

    Action.AddPair(TJSONPair.Create('action', 'game-action'));

    Action.AddPair(TJSONPair.Create('game-action', 'energy-bench'));

    Data.AddPair(TJSONPair.Create('index', index.ToString));

    Data.AddPair(TJSONPair.Create('bench-index', VarToStr(_sender.internal_state)));

    Action.AddPair(TJSONPair.Create('data', Data));

    card_container.Visible := false;

    callback(TModelViewerContext.CreateWithState(Action));
  end;
  card_container.Visible := false;
end;

procedure THandUI.HandleEnergyActive(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'energy-active'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure THandUI.HandleEnergyBenched(Sender: TObject);
var
  I: Integer;
  _temp : TUIButtonStatefull;
begin
  showcardcontainer;
  for I := 0 to length(benched) - 1 do
  begin
    _temp := TUIButtonStatefull.Create(card_container);
    _temp.Parent := card_container;
    _temp.internal_state := i.ToString;
    _temp.Text := RenderCache[benched[i]].Sprite.Data.getvalue<string>('name');
    _temp.OnClick := HandleBenchEnergyAttachment;
    with _temp do
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
end;

procedure THandUI.HandleMoveBench(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'hand-to-bench'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
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

  with btnAction2 do
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

procedure THandUI.HandleToActive(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'init'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure THandUI.handletobench_init(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'init-bench'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure THandUI.ShowCardContainer;
begin
  btnAction1.Visible := false;
  btnAction2.Visible := false;
  if card_container <> nil then freeandnil(card_container);

  card_container := TScrollBox.Create(self);
  card_container.Parent := component_view;
  card_container.Color := rgb(238, 156, 139);
  with card_container do
  begin
    BevelEdges := [];
    BevelOuter := bvNone;
    ParentBackground := false;
    Visible := true;
    align := alClient;
  end;
end;

procedure THandUI.ShowWithFullState(_model: TCardModel; _stage: string;
  _state: TJSONObject; _index: integer; _callback: TProc<TModelViewerContext>);
begin
  inherited ShowWithState(_model, _stage, _callback);
  if _state <> nil then has_state_passed := true else has_state_passed := false;

  state := _state;

  if card_container <> nil then
  begin
    card_container.Visible := false;
  end;

  // if pokemon
  // action1 -> if init -- move to active; else move to bench

  // if trainer
  // action1 -> use (in right stage)

  // if energy
  // btnAction1 -> Attach to active (if active present)
  btnAction1.Visible := false;
  btnAction1.OnClick := nil;
  btnAction2.Visible := false;
  btnAction2.OnClick := nil;

  index := _index;

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
                  //
                  btnAction1.OnClick := handletoactive;
                  btnAction1.Visible := true;
                end;
              end);
            end);
          end
          else if stage = 'init-bench' then
          begin
            model.Sprite.Data.ExistCall('data', procedure(dat: TJSONPair)
            begin
              TJSONObject(dat.JsonValue).ExistCall('stage', procedure (pp: TJSONPair)
              begin
                if pp.JsonValue.Value = '0' then
                begin
                  move_to := hpmActive;
                  btnAction1.Text := 'Move to bench';
                  //
                  btnAction1.OnClick := handletobench_init;
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
                            //
                            btnAction2.OnClick := HandleAttachActive;
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
                          //
                          btnAction1.OnClick := HandleMoveBench;
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
                                (* move_to := hpmBench;
                                btnAction2.Text := 'Attach to bench';
                                //
                                BenchCandidates := indexes;
                                btnAction2.OnClick := HandleAttachBench;
                                btnAction2.Visible := true; *)
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
              //
              btnAction1.OnClick := HandleEnergyActive;
              btnAction1.Visible := true;
              (* btnAction2.Text := 'Attach to benched';
              //
              btnAction2.OnClick := HandleEnergyBenched;
              btnAction2.Visible := true;   *)
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
  ShowWithFullState(_model, _stage, nil, 0, _callback);
end;

{ TBenchUI }

constructor TBenchUI.Create(AOwner: TComponent);
begin
  inherited;
  btnToActive := TUIButton.Create(self);
  btnToActive.Parent := component_view;
  btnToActive.Text := 'Move to active';
  btnToActive.OnClick := HandleToActive;
end;

destructor TBenchUI.Destroy;
begin
  freeandnil(btnToActive);
  inherited;
end;

procedure TBenchUI.HandleResize(Sender: TObject);
begin
  inherited;
  with btnToActive do
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

procedure TBenchUI.HandleToActive(Sender: TObject);
var
  Action, Data: TJSONObject;
begin
  Action := TJSONObject.Create;
  Data := TJSONObject.Create;

  Action.AddPair(TJSONPair.Create('action', 'game-action'));

  Action.AddPair(TJSONPair.Create('game-action', 'set-active'));

  Data.AddPair(TJSONPair.Create('index', index.ToString));

  Action.AddPair(TJSONPair.Create('data', Data));

  callback(TModelViewerContext.CreateWithState(Action));
end;

procedure TBenchUI.ShowWithState(_model: TCardModel; _stage: string;
  _callback: TProc<TModelViewerContext>);
begin
  inherited;
  btnToActive.Visible := false;
  if stage = 'set-active' then
    btnToActive.Visible := true;
end;

end.