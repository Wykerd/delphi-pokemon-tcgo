unit gameLogic;

interface

uses
  Windows, Math, helpers, serverSessions, gameState, cardData, cardDeck, JSON,
  sysutils, dbUnit;

type
  TBroadcastEvent = procedure (s: string) of object;

  TGameLogic = class
  private
    FOnBroadcast: TBroadcastEvent;
    FPlayerSessions: array [0..1] of TClientSession;
    FState: TGameState;
    procedure SetOnBroadcast(const Value: TBroadcastEvent);
    procedure SetState(const Value: TGameState);
  published
    constructor Create(Player1, Player2: TClientSession);
    property State : TGameState read FState write SetState;
    property OnBroadcast : TBroadcastEvent read FOnBroadcast write SetOnBroadcast;
    procedure Init;
    procedure ProcessGameAction (json: TJSONObject; UID: string; data: TJSONObject);
    procedure SendCurrentState;
    procedure GameEnd(winner: integer);
  end;

implementation

{ TGameLogic }

constructor TGameLogic.Create(Player1, Player2: TClientSession);
begin
  FPlayerSessions[0] := Player1;
  FPlayerSessions[1] := Player2;
  State := TGameState.Create;
end;

procedure TGameLogic.GameEnd(winner: integer);
begin
  FPlayerSessions[winner - 1].Socket.Writeln('{"action":"game-end","data":{"win":"You"}}');
  if winner = 1 then  FPlayerSessions[0].Socket.Writeln('{"action":"game-end","data":{"win":"Oponent"}}')
  else FPlayerSessions[1].Socket.Writeln('{"action":"game-end","data":{"win":"Oponent"}}');
  if dmDB.tblUsers.Locate('UID', FPlayerSessions[winner - 1].UID, []) then
  begin
    dmDB.tblUsers.Edit;
    dmDB.tblUsers['Wins'] := dmDB.tblUsers['Wins'] + 1;
  end;
end;

procedure TGameLogic.Init;
var
  I: Integer;
  plr1_has_basic, plr2_has_basic : boolean;
begin
  State.BuildStateFromDecks(FPlayerSessions[0].Deck, FPlayerSessions[1].Deck);

  plr1_has_basic := false;
  plr2_has_basic := false;

  while not (plr1_has_basic AND plr2_has_basic) do
  begin
    // Put hand back to deck
    if not plr1_has_basic then
    begin
      for I := Low(State.Deck1.Hand) to High(State.Deck1.Hand) do
      begin
        SetLength(State.Deck1.Deck, Length(State.Deck1.Deck) + 1);
        State.Deck1.Deck[High(State.Deck1.Deck)] := State.Deck1.Hand[i];
      end;

      SetLength(State.Deck1.Hand, 0);

      State.Deck1.Shuffle;
    end;

    if not plr2_has_basic then
    begin
      for I := Low(State.Deck2.Hand) to High(State.Deck2.Hand) do
      begin
        SetLength(State.Deck2.Deck, Length(State.Deck2.Deck) + 1);
        State.Deck2.Deck[High(State.Deck2.Deck)] := State.Deck2.Hand[i];
      end;

      SetLength(State.Deck2.Hand, 0);

      State.Deck2.Shuffle;
    end;

    // do pulling of cards
    if not plr1_has_basic then
    for I := 0 to 6 do
    begin
      setlength(State.Deck1.Hand, length(State.Deck1.Hand) + 1);
      State.Deck1.Hand[high(State.Deck1.Hand)] := State.Deck1.Deck[high(State.Deck1.Deck)];

      if TCardRecord(State.Deck1.Hand[high(State.Deck1.Hand)]).CardType = ctPokemon then
        if TPokemonData(TCardRecord(State.Deck1.Hand[high(State.Deck1.Hand)]).data).stage = 0 then
          plr1_has_basic := true;

      setlength(State.Deck1.Deck, length(State.Deck1.Deck) - 1);
    end;

    if not plr2_has_basic then
    for i := 0 to 6 do
    begin
      setlength(State.Deck2.Hand, length(State.Deck2.Hand) + 1);
      State.Deck2.Hand[high(State.Deck2.Hand)] := State.Deck2.Deck[high(State.Deck2.Deck)];

      if TCardRecord(State.Deck2.Hand[high(State.Deck2.Hand)]).CardType = ctPokemon then
        if TPokemonData(TCardRecord(State.Deck2.Hand[high(State.Deck2.Hand)]).data).stage = 0 then
          plr2_has_basic := true;

      setlength(State.Deck2.Deck, length(State.Deck2.Deck) - 1);
    end;

    // DEBUG ONLY! //
    {
      State.Deck1.Active := State.Deck1.Deck[0];
      State.Deck2.Active := State.Deck2.Deck[0];

      setlength(State.Deck1.Discard, 1);
      State.Deck1.Discard[0] := State.Deck1.Deck[0];
      setlength(State.Deck2.Discard, 1);
      State.Deck2.Discard[0] := State.Deck2.Deck[0];

      setlength(State.Deck1.Bench, 1);
      State.Deck1.Bench[0] := State.Deck1.Deck[1];
      setlength(State.Deck2.Bench, 1);
      State.Deck2.Bench[0] := State.Deck2.Deck[1];
    }
  end;

  // Pull the prize cards
  for I := 0 to 5 do
  begin
    setlength(State.Deck1.PrizeCards, i + 1);
    setlength(State.Deck2.PrizeCards, i + 1);

    State.Deck1.PrizeCards[i] := State.Deck1.Deck[high(State.Deck1.Deck)];
    setlength(State.Deck1.Deck, length(State.Deck1.Deck) - 1);

    State.Deck2.PrizeCards[i] := State.Deck2.Deck[high(State.Deck2.Deck)];
    setlength(State.Deck2.Deck, length(State.Deck2.Deck) - 1);
  end;

  FPlayerSessions[0].Socket.Writeln('{"action":"game-start","data":' + State.ToJSON(0).ToString + '}');
  FPlayerSessions[1].Socket.Writeln('{"action":"game-start","data":' + State.ToJSON(1).ToString + '}');
end;

procedure TGameLogic.ProcessGameAction(json: TJSONObject; UID: string; data: TJSONObject);
var
  Session: TClientSession;
  index: integer;
  _temp_sessions : TSessionsArr;
  damage : integer;
  i: integer;
  _temp_record : TCardRecord;
begin
  // Create dynamic array
  setlength(_temp_sessions, 2);
  _temp_sessions[0] := FPlayerSessions[0];
  _temp_sessions[1] := FPlayerSessions[1];

  index := GetUserFromUID(UID, _temp_sessions);

  if index > -1 then
  begin
    Session := _temp_sessions[index];
    json.ExistCall('game-action', procedure (action_pair: TJSONPair)
    var
      action: string;
      init_index, i: integer;
    begin
      action := action_pair.JsonValue.Value;

      if State.turn <> index then
      begin
        Session.Socket.WriteLn('{"action":"error","data":{"details":"oponent_turn"}}');
      end

      else if action = 'init' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(init)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctPokemon then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(init)"}}');
              exit;
            end;
            if TPokemonData(Session.Deck.Hand[init_index].Data).stage <> 0 then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_stage(init)"}}');
              exit;
            end;
            (* if Session.Deck.Active <> nil then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"active_not_nil(init)"}}');
              exit;
            end; *)

            Session.Deck.Active := Session.Deck.Hand[init_index];

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            State.stage := 'init-bench';

            FPlayerSessions[0].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(0).ToString + '}');
            FPlayerSessions[1].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(1).ToString + '}');
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(init)"}}');
          end;
        end;
      end

      else if (action = 'init-bench') or (action = 'hand-to-bench') then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(init-bench)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctPokemon then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(init-bench)"}}');
              exit;
            end;
            if TPokemonData(Session.Deck.Hand[init_index].Data).stage <> 0 then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_stage(init-bench)"}}');
              exit;
            end;
            if length(Session.Deck.Bench) >= 5 then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"max_bench_reached(init-bench)"}}');
              exit;
            end;

            setlength(Session.Deck.Bench, length(Session.Deck.Bench) + 1);
            Session.Deck.Bench[High(Session.Deck.Bench)] := Session.Deck.Hand[init_index];

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            if action = 'init-bench' then State.stage := 'init-bench' else State.BuildTurnStage;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(init)"}}');
          end;
        end;
      end

      else if action = 'evolve' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(evolve)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctPokemon then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(evolve)"}}');
              exit;
            end;
            if TPokemonData(Session.Deck.Hand[init_index].Data).stage <> TPokemonData(Session.Deck.Active.data).stage + 1 then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_stage(evolve)"}}');
              exit;
            end;
            if TPokemonData(Session.Deck.Hand[init_index].Data).base <> TPokemonData(Session.Deck.Active.data).id  then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card(evolve)"}}');
              exit;
            end;

            TPokemonData(Session.Deck.Hand[init_index].Data).AttachedStage := Session.Deck.Active;
            Session.Deck.Active := Session.Deck.Hand[init_index];

            for I := 0 to length(TPokemonData(Session.Deck.Active.Data).AttachedStage.AttachedEnergy) - 1 do
            begin
              setlength(session.Deck.Active.AttachedEnergy, length(session.Deck.Active.AttachedEnergy) + 1);
              session.Deck.Active.AttachedEnergy[high(session.Deck.Active.AttachedEnergy)] :=
                TPokemonData(Session.Deck.Active.Data).AttachedStage.AttachedEnergy[i];
            end;

            setlength(TPokemonData(Session.Deck.Active.Data).AttachedStage.AttachedEnergy, 0);

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            State.BuildTurnStage;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(init)"}}');
          end;
        end;
      end

      else if action = 'retreat' then
      begin
        if length(session.Deck.Bench) < 1 then
        begin
          Session.Socket.WriteLn('{"action":"error","data":{"details":"no_benched(retreat)"}}');
          exit;
        end;
        if TPokemonData(Session.Deck.Active.data).RetreatCost > length(Session.Deck.Active.AttachedEnergy) then
        begin
          Session.Socket.WriteLn('{"action":"error","data":{"details":"retreat_cost_no_met(retreat)"}}');
          exit;
        end;

        for I := 0 to TPokemonData(Session.Deck.Active.data).RetreatCost do
        begin
          setlength(Session.Deck.Discard, length(Session.Deck.Discard) + 1);
          // Session.Deck.Discard[HIGH(Session.Deck.Discard)]
          //  := Session.Deck.Active.AttachedEnergy[High(Session.Deck.Active.AttachedEnergy)];
          setlength(Session.Deck.Active.AttachedEnergy, length(Session.Deck.Active.AttachedEnergy) - 1);
        end;

        setlength(session.Deck.Bench, length(session.Deck.Bench) + 1);
        session.Deck.Bench[high(session.Deck.Bench)] := Session.Deck.Active;
        Session.Deck.Active := nil;
        State.turn_retreat := false;
        State.stage := 'set-active';
        SendCurrentState;
      end

      else if action = 'set-active' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Bench) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(set-active)"}}');
              exit;
            end;
            if Session.Deck.Bench[init_index].CardType <> ctPokemon then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(set-active)"}}');
              exit;
            end;
            if Session.Deck.Active <> nil then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"active_not_nil(set-active)"}}');
              exit;
            end;

            Session.Deck.Active := Session.Deck.Bench[init_index];

            for I := init_index to length(session.Deck.Bench) - 2 do
              Session.Deck.Bench[i] := Session.Deck.Bench[i + 1];

            setlength(Session.Deck.Bench, length(session.Deck.Bench) - 1);

            State.BuildTurnStage;

            FPlayerSessions[0].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(0).ToString + '}');
            FPlayerSessions[1].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(1).ToString + '}');
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(set-active)"}}');
          end;
        end;
      end

      else if action = 'use-attack' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if (init_index > 2) or (init_index < 1) then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(use-attack)"}}');
              exit;
            end;
            if init_index = 1 then
            begin
              if TPokemonData(Session.Deck.Active.data).Attack1 = nil then
              begin
                Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(use-attack)"}}');
                exit;
              end;
              if TPokemonData(Session.Deck.Active.data).Attack1.UseMultiplier then
              begin
                if TPokemonData(Session.Deck.Active.data).Attack1.CoinFlip then
                begin
                  if Random > 0.5 then
                  begin
                    damage := TPokemonData(Session.Deck.Active.data).Attack1.Multiplier.Calculate(
                      TPokemonData(Session.Deck.Active.data).Attack1.damage);
                  end;
                end
                else
                begin
                  damage := TPokemonData(Session.Deck.Active.data).Attack1.damage;
                end;
              end
              else
              begin
                damage := TPokemonData(Session.Deck.Active.data).Attack1.damage;
              end;
            end
            else
            begin
              if TPokemonData(Session.Deck.Active.data).Attack2 = nil then
              begin
                Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(use-attack)"}}');
                exit;
              end;
              if TPokemonData(Session.Deck.Active.data).Attack2.UseMultiplier then
              begin
                if TPokemonData(Session.Deck.Active.data).Attack2.CoinFlip then
                begin
                  if Random > 0.5 then
                  begin
                    damage := TPokemonData(Session.Deck.Active.data).Attack2.Multiplier.Calculate(
                      TPokemonData(Session.Deck.Active.data).Attack2.damage);
                  end
                  else
                  begin
                    damage := TPokemonData(Session.Deck.Active.data).Attack2.damage;
                  end;
                end
                else
                begin
                  damage := TPokemonData(Session.Deck.Active.data).Attack2.damage;
                end;
              end
              else
              begin
                damage := TPokemonData(Session.Deck.Active.data).Attack2.damage;
              end;
            end;

            if State.turn = 0 then state.turn := 1
            else state.turn := 0;

            State.StartTurn;
            if State.turn = 1 then
            begin
              if length(State.Deck2.Deck) < 1 then
              begin
                GameEnd(1);
                exit;
              end;
              setlength(State.Deck2.Hand, length(State.Deck2.Hand) + 1);
              State.Deck2.Hand[high(State.Deck2.Hand)] := State.Deck2.Deck[high(state.Deck2.Deck)];
              setlength(State.Deck2.Deck, length(state.Deck2.Deck) - 1);
            end
            else
            begin
              if length(State.Deck1.Deck) < 1 then
              begin
                GameEnd(2);
                exit;
              end;
              setlength(State.Deck1.Hand, length(State.Deck1.Hand) + 1);
              State.Deck1.Hand[high(State.Deck1.Hand)] := State.Deck1.Deck[high(state.Deck1.Deck)];
              setlength(State.Deck1.Deck, length(state.Deck1.Deck) - 1);
            end;

            TPokemonData(FPlayerSessions[State.turn].Deck.Active.data).hp := TPokemonData(FPlayerSessions[State.turn].Deck.Active.data).hp - damage;

            if TPokemonData(FPlayerSessions[State.turn].Deck.Active.data).hp <= 0 then
            begin
              if State.turn = 0 then i := 1
              else i := 0;

              setlength(FPlayerSessions[i].Deck.Hand, length(FPlayerSessions[i].Deck.Hand) + 1);

              FPlayerSessions[i].Deck.Hand[high(FPlayerSessions[i].Deck.Hand)] :=
                  FPlayerSessions[i].Deck.PrizeCards[high(FPlayerSessions[i].Deck.PrizeCards)];

              setlength(FPlayerSessions[i].Deck.PrizeCards, length(FPlayerSessions[i].Deck.PrizeCards) - 1);

              if length(FPlayerSessions[i].Deck.PrizeCards) = 0 then
              begin
                GameEnd(i);
                exit;
              end;

              setlength(FPlayerSessions[State.turn].Deck.Discard, length(FPlayerSessions[State.turn].Deck.Discard) + 1);

              FPlayerSessions[State.turn].Deck.Discard[high(FPlayerSessions[State.turn].Deck.Discard)]
                := FPlayerSessions[State.turn].Deck.Active;

              if length(FPlayerSessions[State.turn].Deck.Bench) < 1 then
              begin
                GameEnd(i);
                exit;
              end;


              FPlayerSessions[State.turn].Deck.Active := nil;

              State.stage := 'set-active';
            end;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(use-attack)"}}');
          end;
        end;
      end
      (*
      else if action = 'evolve-bench' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if data.Exists('bench-index') then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_bench_index(evolve-bench)"}}');
              exit;
            end;
            if length(Session.Deck.Bench) <= strtoint(data.Get('bench-index').JsonValue.Value) then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_bench_index(evolve-bench)"}}');
              exit;
            end;
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(evolve-bench)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctPokemon then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(evolve-bench)"}}');
              exit;
            end;
            _temp_record := Session.Deck.Bench[strtoint(data.Get('bench-index').JsonValue.Value)];
            if TPokemonData(Session.Deck.Hand[init_index].Data).stage <> TPokemonData(_temp_record.data).stage + 1 then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_stage(evolve-bench)"}}');
              exit;
            end;
            if TPokemonData(Session.Deck.Hand[init_index].Data).base <> TPokemonData(_temp_record.data).id  then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card(evolve-bench)"}}');
              exit;
            end;

            TPokemonData(Session.Deck.Hand[init_index].Data).AttachedStage := _temp_record;
            _temp_record := Session.Deck.Hand[init_index];

            for I := 0 to length(TPokemonData(_temp_record.Data).AttachedStage.AttachedEnergy) - 1 do
            begin
              setlength(_temp_record.AttachedEnergy, length(_temp_record.AttachedEnergy) + 1);
              _temp_record.AttachedEnergy[high(_temp_record.AttachedEnergy)] :=
                TPokemonData(_temp_record.Data).AttachedStage.AttachedEnergy[i];
            end;

            setlength(TPokemonData(_temp_record.Data).AttachedStage.AttachedEnergy, 0);

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            State.BuildTurnStage;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(evolve-bench)"}}');
          end;
        end;
      end

      else if action = 'energy-bench' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(energy-bench)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctEnergy then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(energy-bench)"}}');
              exit;
            end;
            if data.Exists('bench-index') then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_bench_index_does_not_exist(energy-bench)"}}');
              exit;
            end;
            if length(Session.Deck.Bench) <= strtoint(data.Get('bench-index').JsonValue.Value) then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_bench_index(energy-bench)"}}');
              exit;
            end;
            _temp_record := Session.Deck.Bench[strtoint(data.Get('bench-index').JsonValue.Value)];

            setlength(_temp_record.AttachedEnergy, length(_temp_record.AttachedEnergy) + 1);
            _temp_record.AttachedEnergy[high(_temp_record.AttachedEnergy)] := Session.Deck.Hand[init_index];

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            State.turn_energy := false;
            State.BuildTurnStage;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(energy-bench)"}}');
          end;
        end;
      end        *)

      (* else if action = 'trainer' then
      begin

      end *)

      else if action = 'energy-active' then
      begin
        if data.Exists('index') then
        begin
          try
            init_index := strtoint(data.Get('index').JsonValue.Value);
            if length(session.Deck.Hand) <= init_index then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(energy-active)"}}');
              exit;
            end;
            if Session.Deck.Hand[init_index].CardType <> ctEnergy then
            begin
              Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_card_type(energy-active)"}}');
              exit;
            end;

            setlength(Session.Deck.Active.AttachedEnergy, length(Session.Deck.Active.AttachedEnergy) + 1);
            Session.Deck.Active.AttachedEnergy[high(Session.Deck.Active.AttachedEnergy)] := Session.Deck.Hand[init_index];

            for I := init_index to length(session.Deck.Hand) - 2 do
              Session.Deck.Hand[i] := Session.Deck.Hand[i + 1];

            setlength(Session.Deck.Hand, length(session.Deck.Hand) - 1);

            State.turn_energy := false;
            State.BuildTurnStage;

            SendCurrentState;
          except
            Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_index(energy-active)"}}');
          end;
        end;
      end

      else if action = 'end-turn' then
      begin
        if not ((State.stage = 'init') or (State.stage = 'set-active')) then
        begin
          if State.stage = 'init-bench' then
          begin
            if State.turn = 0 then State.stage := 'init'
            else
            begin
              State.StartTurn;
              if State.turn = 0 then
              begin
                if length(State.Deck2.Deck) < 1 then
                begin
                  GameEnd(1);
                  exit;
                end;
                setlength(State.Deck2.Hand, length(State.Deck2.Hand) + 1);
                State.Deck2.Hand[high(State.Deck2.Hand)] := State.Deck2.Deck[high(state.Deck2.Deck)];
                setlength(State.Deck2.Deck, length(state.Deck2.Deck) - 1);
              end
              else
              begin
                if length(State.Deck2.Deck) < 1 then
                begin
                  GameEnd(2);
                  exit;
                end;
                setlength(State.Deck1.Hand, length(State.Deck1.Hand) + 1);
                State.Deck1.Hand[high(State.Deck1.Hand)] := State.Deck1.Deck[high(state.Deck1.Deck)];
                setlength(State.Deck1.Deck, length(state.Deck1.Deck) - 1);
              end;
            end;
          end
          else
          begin
            State.StartTurn;
            if State.turn = 0 then
            begin
              if length(State.Deck2.Deck) < 1 then
                begin
                  GameEnd(1);
                  exit;
                end;
              setlength(State.Deck2.Hand, length(State.Deck2.Hand) + 1);
              State.Deck2.Hand[high(State.Deck2.Hand)] := State.Deck2.Deck[high(state.Deck2.Deck)];
              setlength(State.Deck2.Deck, length(state.Deck2.Deck) - 1);
            end
            else
            begin
              if length(State.Deck2.Deck) < 1 then
                begin
                  GameEnd(2);
                  exit;
                end;
              setlength(State.Deck1.Hand, length(State.Deck1.Hand) + 1);
              State.Deck1.Hand[high(State.Deck1.Hand)] := State.Deck1.Deck[high(state.Deck1.Deck)];
              setlength(State.Deck1.Deck, length(state.Deck1.Deck) - 1);
            end;
          end;

          if State.turn = 0 then state.turn := 1
          else state.turn := 0;
        end;
        SendCurrentState;
      end

      else
      begin
        Session.Socket.WriteLn('{"action":"error","data":{"details":"invalid_game_action"}}');
      end;
    end);
  end;
end;

procedure TGameLogic.SendCurrentState;
begin
  FPlayerSessions[0].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(0).ToString + '}');
  FPlayerSessions[1].Socket.Writeln('{"action":"game-state","data":' + State.ToJSON(1).ToString + '}');
end;

procedure TGameLogic.SetOnBroadcast(const Value: TBroadcastEvent);
begin
  FOnBroadcast := Value;
end;

procedure TGameLogic.SetState(const Value: TGameState);
begin
  FState := Value;
end;

end.
