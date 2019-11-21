unit cardData;

interface

uses
  System.JSON, dbunit, sysutils, helpers, dialogs, classes;

type
  TCardType = (ctPokemon, ctTrainer, ctEnergy);

  TCardEnergy = (ceWater, cePsychic, ceGrass, ceFire,
    ceFighting, ceElectric, ceColorless, ceUndef);

  TCardBase = class (TPersistent)
    function ToJSON: TJSONObject; virtual; abstract;
  end;

  // implimented + json
  TCardSet = class (TCardBase)
  published
    constructor CreateFromID(id: integer);
    function ToJSON: TJSONObject; override;
  public
    id: integer;
    name: string;
    quantity: integer;
  end;

  // BASE TYPE
  TCardData = class (TCardBase)
  published
    constructor CreateFromID(id: integer); virtual; abstract;
  public
    id: integer;
    image: string;
  end;

  // implimented + string
  TLogicMultiplier = class (TObject)
  published
    constructor Parse(s: string);
    function Calculate(i: integer): integer;
    function ToString: string;
  public
    Multiplier: char;
    Value: integer;
  end;

  // implimented + json
  TCardRecord = class (TCardBase)
  published
    constructor CreateFromID(id: integer);
    function ToJSON: TJSONObject; override;
  public
    id: integer;
    CardType: TCardType;
    CardSet: TCardSet;
    SetNumber: integer;
    Rarity: byte;
    name: string;
    data: TCardData;
    AttachedEnergy: array of TCardRecord;
  end;

  //implimented + json
  TPokemonAttack = class (TCardBase)
  published
    constructor CreateFromID(id: integer);
    function ToJSON: TJSONObject; override;
  public
    id: integer;
    name: string;
    energy: array of TCardEnergy;
    damage: integer;
    UseEffect: boolean;
    UseMultiplier: boolean;
    CoinFlip: boolean;
    Effect: string;
    Multiplier: TLogicMultiplier;
  end;

  TPokemonType = (ptWater, ptPsychic, ptGrass, ptFire,
    ptFighting, ptElectric, ptColorless, ptUndef);

  // implimented  + json
  TPokemonData = class (TCardData)
  published
    constructor CreateFromID(id: integer); override;
    function ToJSON: TJSONObject; override;
  public
    PokemonType: TPokemonType;
    resistance: TPokemonType;
    weakness: TPokemonType;
    ResistanceMultiplier: TLogicMultiplier;
    WeaknessMultiplier: TLogicMultiplier;
    RetreatCost: integer;
    Attack1 : TPokemonAttack;
    Attack2 : TPokemonAttack;
    Base: integer;
    BaseImage: string;
    stage: byte;
    hp: integer;
    AttachedStage: TCardRecord;
  end;

  // implimented + json
  TTrainerData = class (TCardData)
  published
    constructor CreateFromID(id: integer); override;
    function ToJSON: TJSONObject; override;
  public
    CardFunction: string;
    Description: string;
  end;

function VarToEnergyType(s: Variant) : TCardEnergy;

implementation

{ TCardRecord }

function VarToEnergyType(s: Variant) : TCardEnergy;
begin
  if s = 'water' then exit(ceWater);
  if s = 'psychic' then exit(cePsychic);
  if s = 'grass' then exit(ceGrass);
  if s = 'fire' then exit(ceFire);
  if s = 'fighting' then exit(ceFighting);
  if s = 'electric' then exit(ceElectric);
  if s = 'colorless' then exit(ceColorless);

  exit(ceUndef);
end;

function EnergyToString(ce: TCardEnergy): string;
begin
  if ce = ceWater then exit('water');
  if ce = cePsychic then exit('psychic');
  if ce = ceGrass then exit('grass');
  if ce = ceFire then exit('fire');
  if ce = ceFighting then exit('fighting');
  if ce = ceElectric then exit('electric');
  if ce = ceColorless then exit('colorless');

  exit('colorless');
end;

function StringToCardType(s: string) : TCardType;
begin
  if s = 'pokemon' then exit(ctPokemon);
  if s = 'trainer' then exit(ctTrainer);
  if s = 'energy' then exit(ctEnergy);

  raise Exception.Create('Invalid card type in the database! Are you using an modded database?');
end;

function CardTypeToString(ct: TCardType) : string;
begin
  if ct = ctPokemon then exit('pokemon');
  if ct = ctTrainer then exit('trainer');
  if ct = ctEnergy then exit('energy');

  raise Exception.Create('Card type to string failed. This should not fail! Do not release to production if this does not pass!');
end;

constructor TCardRecord.CreateFromID(id: integer);
begin
  SetLength(AttachedEnergy, 0);

  with dmDB do
  begin
    if not tblCards.Locate('ID', id, []) then
      raise Exception.Create('404, There is no card in the database with that ID');

    //showmessagefmt('%d is now being created', [id]);
    self.id := id;
    CardType := StringToCardType(tblCards['Type']);
    CardSet := TCardSet.CreateFromID(tblCards['Set']);
    SetNumber := tblCards['SetNumber'];
    Rarity := tblCards['Rarity'];
    self.name := tblCards['CardName'];
    if CardType = ctPokemon then data := TPokemonData.CreateFromID(tblCards['Data'])
    else if CardType = ctTrainer then data := TTrainerData.CreateFromID(tblCards['Data'])
    else data := nil;
  end;
end;

function TCardRecord.ToJSON: TJSONObject;
var
  arr : TJSONArray;
  e: TCardRecord;
begin
  result := TJSONObject.Create;
  arr := TJSONArray.Create;
  for e in AttachedEnergy do
  begin
    arr.Add(e.name);
  end;

  with result do
  begin
    AddPair(TJSONPair.Create('id', inttostr(self.id)));
    AddPair(TJSONPair.Create('type', CardTypeToString(CardType)));
    AddPair(TJSONPair.Create('set', CardSet.ToJSON));
    AddPair(TJSONPair.Create('set-number', inttostr(SetNumber)));
    AddPair(TJSONPair.Create('rarity', inttostr(Rarity)));
    AddPair(TJSONPair.Create('name', Name));
    if data <> nil then
      AddPair(TJSONPair.Create('data', data.ToJSON));
    if length(AttachedEnergy) > 0 then
      AddPair(TJSONPair.Create('attached-energy', arr));
  end;
end;

{ TPokemonData }

function VarToPkmType(s: Variant) : TPokemonType;
begin
  if s = 'water' then exit(ptWater);
  if s = 'psychic' then exit(ptPsychic);
  if s = 'grass' then exit(ptGrass);
  if s = 'fire' then exit(ptFire);
  if s = 'fighting' then exit(ptFighting);
  if s = 'electric' then exit(ptElectric);
  if s = 'colorless' then exit(ptColorless);

  exit(ptUndef);
end;

function PkmTypeToString(pt: TPokemonType): string;
begin
  if pt = ptWater then exit('water');
  if pt = ptPsychic then exit('psychic');
  if pt = ptGrass then exit('grass');
  if pt = ptFire then exit('fire');
  if pt = ptFighting then exit('fighting');
  if pt = ptElectric then exit('electric');
  if pt = ptColorless then exit('colorless');

  exit('colorless');
end;

constructor TPokemonData.CreateFromID(id: integer);
begin
  with dmDB do
  begin
    if not tblPokemon.Locate('ID', id, []) then
      raise Exception.Create('404, There is no pokemon in the database with that ID');

    self.id := id;
    PokemonType := VarToPkmType(tblPokemon['Type']);
    resistance := VarToPkmType(tblPokemon['Resistance']);
    weakness := VarToPkmType(tblPokemon['Weakness']);

    if not UndefinedVar(tblPokemon['ResistanceMultiplier']) then
      ResistanceMultiplier := TLogicMultiplier.Parse(tblPokemon['ResistanceMultiplier'])
    else
      ResistanceMultiplier := nil;

    if not UndefinedVar(tblPokemon['WeaknessMultiplier']) then
      WeaknessMultiplier := TLogicMultiplier.Parse(tblPokemon['WeaknessMultiplier'])
    else
      WeaknessMultiplier := nil;

    RetreatCost := tblPokemon['RetreatCost'];
    Attack1 := TPokemonAttack.CreateFromID(tblPokemon['Attack1']);
    if tblPokemon['Attack2'] <> 0 then Attack2 := TPokemonAttack.CreateFromID(tblPokemon['Attack2'])
    else Attack2 := nil;
    Base := tblPokemon['Base'];
    Stage := tblPokemon['Stage'];
    HP := tblPokemon['HP'];
    Image := tblPokemon['Image'];

    if tblPokemon.Locate('ID', base, []) then
      if not UndefinedVar(tblPokemon['Image']) then
        BaseImage := tblPokemon['Image']
      else
        BaseImage := 'undef';
  end;
end;

function TPokemonData.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create;
  with result do
  begin
    AddPair(TJSONPair.Create('id', inttostr(self.id)));
    AddPair(TJSONPair.Create('type', PkmTypeToString(self.PokemonType)));
    if resistance <> ptUndef then AddPair(TJSONPair.Create('resistance', PkmTypeToString(resistance)));
    if weakness <> ptUndef then AddPair(TJSONPair.Create('weakness', PkmTypeToString(weakness)));
    if ResistanceMultiplier <> nil then AddPair(TJSONPair.Create('resistance-multiplier', ResistanceMultiplier.ToString));
    if WeaknessMultiplier <> nil then AddPair(TJSONPair.Create('weakness-multiplier', WeaknessMultiplier.ToString));
    AddPair(TJSONPair.Create('retreat-cost', inttostr(RetreatCost)));
    AddPair(TJSONPair.Create('attack1', Attack1.ToJSON));
    if Attack2 <> nil then AddPair(TJSONPair.Create('attack2', Attack2.ToJSON));
    if base > 0 then
    begin
      AddPair(TJSONPair.Create('base', inttostr(base)));
      AddPair(TJSONPair.Create('base-image', BaseImage));
    end;
    AddPair(TJSONPair.Create('stage', inttostr(stage)));
    AddPair(TJSONPair.Create('hp', inttostr(hp)));
    AddPair(TJSONPair.Create('image', image));

    if AttachedStage <> nil then
      AddPair(TJSONPair.Create('attached-stage', AttachedStage.ToJSON));

  end;
end;

{ TPokemonAttack }

constructor TPokemonAttack.CreateFromID(id: integer);
var
  energy_temp : TArray<string>;
  s: string;
begin
  SetLength(energy, 0);

  with dmDB do
  begin
    if not tblAttacks.Locate('ID', id, []) then
      raise Exception.Create('404, There is no attack in the database with that ID');

    self.id := id;
    self.name := tblAttacks['AttackName'];

    // populate the energy
    setlength(energy, 0);

    energy_temp := strSplit(tblAttacks['Energy'], ',');
    for s in energy_temp do
    begin
      setlength(energy, length(energy) + 1);
      energy[high(energy)] := VarToEnergyType(s);
    end;

    damage := tblAttacks['Damage'];

    UseEffect := not UndefinedVar(tblAttacks['Effect']);
    if UseEffect then
      Effect := tblAttacks['Effect'];

    UseMultiplier := not UndefinedVar(tblAttacks['Multiplier']);
    if UseMultiplier then
      Multiplier := TLogicMultiplier.Parse(String(tblAttacks['Multiplier']))
    else
      Multiplier := nil;

    CoinFlip := tblAttacks['CoinFlip'];
  end;
end;

function TPokemonAttack.ToJSON: TJSONObject;
var
  arr : TJSONArray;
  e: TCardEnergy;
begin
  result := TJSONObject.Create;
  arr := TJSONArray.Create;
  for e in energy do
  begin
    arr.Add(EnergyToString(e));
  end;

  with result do
  begin
    AddPair(TJSONPair.Create('id', inttostr(self.id)));
    AddPair(TJSONPair.Create('name', self.name));
    AddPair(TJSONPair.Create('energy', arr));
    AddPair(TJSONPair.Create('damage', inttostr(self.damage)));
    if self.UseEffect then AddPair(TJSONPair.Create('effect', self.Effect));
    if self.UseMultiplier then AddPair(TJSONPair.Create('multiplier', Multiplier.ToString));
    if CoinFlip then AddPair(TJSONPair.Create('coin-flip', '1'))
    else AddPair(TJSONPair.Create('coin-flip', '0'));
  end;
end;

{ TTrainerData }

constructor TTrainerData.CreateFromID(id: integer);
begin

  with dmDB do
  begin
    if not tblTrainers.Locate('ID', id, []) then
      raise Exception.Create('404, There is no trainer in the database with that ID');

    self.id := id;
    image := tblTrainers['Image'];
    CardFunction := tblTrainers['Function'];
    Description := tblTrainers['Description'];
  end;

end;

function TTrainerData.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create;
  with result do
  begin
    AddPair(TJSONPair.Create('id', inttostr(self.id)));
    AddPair(TJSONPair.Create('image', image));
    AddPair(TJSONPair.Create('function', CardFunction));
    AddPair(TJSONPair.Create('description', Description));
  end;
end;

{ TCardSet }

constructor TCardSet.CreateFromID(id: integer);
begin
  with dmDB do
  begin
    tblSets.open;
    if not tblSets.Locate('ID', id, []) then
      raise Exception.Create('404, There is no set in the database with that ID');

    self.id := id;
    self.name := tblSets['SetName'];
    quantity := tblSets['Quantity'];
  end;
end;

function TCardSet.ToJSON: TJSONObject;
begin
  result := TJSONObject.Create;
  with result do
  begin
    AddPair(TJSONPair.Create('id', inttostr(self.id)));
    AddPair(TJSONPair.Create('name', name));
    AddPair(TJSONPair.Create('quantity', inttostr(quantity)));
  end;
end;

{ TLogicMultiplier }

function TLogicMultiplier.Calculate(i: integer): integer;
begin
  case UpCase(multiplier) of
    'X': exit(i * value);
    '+': exit(i + value);
    '-': exit(i - value);
    '/': exit(round(i / value));
    else exit(i);
  end;
end;

constructor TLogicMultiplier.Parse(s: string);
begin
  multiplier := s[1];
  delete(s, 1, 1);
  value := strtoint(s);
end;

function TLogicMultiplier.ToString: string;
begin
  result := format('%s%d', [Multiplier, Value]);
end;

end.
