unit pkmCard;

interface

uses
  Windows, OpenGL, Graphics, Classes, Math, SysUtils, StrUtils, System.JSON,
  helpers, Textures, IdGlobal, idhash, IdHashSHA;

type
  TCardSprite = class(TBitmap)
  private
    FTexture: GLuint;
    FData: TJSONObject;
    FAssetsDir: String;
    procedure SetData(const Value: TJSONObject);
    procedure SetTexture(const Value: GLuint);
    procedure SetAssetsDir(const Value: String);
    procedure RenderAsPokemon;
    procedure RenderAsEnergy;
    procedure RenderAsTrainer;
    procedure RenderUnknown;
  published
    constructor Create; override;
    destructor Destroy; override;
    property Texture: GLuint read FTexture write SetTexture;
    property Data: TJSONObject read FData write SetData;
    property AssetsDir: String read FAssetsDir write SetAssetsDir;
    procedure Render;
    procedure GenerateTexture;
  public
    hash: string;
    class function decodeType(s: string): string;
  end;

  TCardModelType = (modelActive, modelDefault);

  TCardModel = class
  private
    FModelType: TCardModelType;
    FSprite: TCardSprite;
    procedure SetModelType(const Value: TCardModelType);
    procedure SetSprite(const Value: TCardSprite);
  published
    constructor Create;
    destructor Destroy;
    property Sprite : TCardSprite read FSprite write SetSprite;
    property ModelType : TCardModelType read FModelType write SetModelType;
    procedure Draw(RenderType: TCardModelType = modelDefault);
  public
    name : integer; // store some value; used for locating the object during events
    index : integer; // store the index of card in state if applicable;
    class procedure RenderBackCard;
  end;

var
  CARD_BACK,
  CARD_BACK_ENERGY,
  CARD_BACk_TRAINER,
  CARD_STAGE : TBitmap;

  MODEL_SIDE_TEX: GLUint;
  MODEL_BACK_TEX: GLUint;

procedure RELOADPKMCARDVAR;
procedure LoadPkmModelTextures;

implementation

{ TCardSprite }

constructor TCardSprite.Create;
begin
  inherited;
  Height := 590;
  Width := 420;
  AssetsDir := GetCurrentDir + '\client\assets\cards';
end;

class function TCardSprite.decodeType(s: string): string;
const
  types: array [0 .. 6] of string = ('water', 'psychic', 'grass', 'fire',
    'fighting', 'electric', 'colorless');
begin
  result := s;
  // Default to colorless if not found
  if not MatchStr(s, types) then
    result := 'colorless';
end;

destructor TCardSprite.Destroy;
begin
  glDeleteTextures(1, FTexture);
  inherited;
end;


// Do not generate textures in the same thread as OpenGL
procedure TCardSprite.GenerateTexture;
var
  dat : Array of LongWord;
  W : Integer;
  H : Integer;
  C : LongWord;
  Line : ^LongWord;
begin
  // The following code is adapted from the dependency "Textures" function
  // LoadJPGTexture!

  SetLength(dat, Width*Height);

  For H:=0 to Height-1 do
  Begin
    Line := scanline[Height-H-1];   // flip image
    For W:=0 to Width-1 do
    Begin
      c:=Line^ and $FFFFFF; // Need to do a color swap
      dat[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;  // 4 channel.
      inc(Line);
    End;
  End;

  TThread.Synchronize(nil, procedure
  begin
    // delete the existing texture
    glDeleteTextures(1, FTexture);
    FTexture := CreateTexture(Width, Height, GL_RGBA, addr(dat[0]));
  end);
end;

procedure TCardSprite.Render;
var
  card_type : string;
begin
  if Data.Exists('type') then
  begin
    card_type := LowerCase(data.Get('type').JsonValue.Value);
    if length(card_type) > 0 then
    begin
      if card_type = 'pokemon' then RenderAsPokemon
      else if card_type = 'trainer' then RenderAsTrainer
      else if card_type = 'energy' then RenderAsEnergy
      else RenderUnknown;
    end;
  end;

  GenerateTexture;
end;

procedure TCardSprite.RenderAsEnergy;
var
  bitmap : TBitmap;
  function GetImage : string;
  var
    energyType : string;
  begin
    if data.Exists('name') then
    begin
      energyType := data.Get('name').JsonValue.Value;
      if energyType = 'water' then exit('0-5-2');
      if energyType = 'psychic' then exit('0-5-5');
      if energyType = 'grass' then exit('0-5-0');
      if energyType = 'fire' then exit('0-5-1');
      if energyType = 'fighting' then exit('0-5-4');
      if energyType = 'electric' then exit('0-5-3');
      exit('1-5-6');
    end;
  end;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  bitmap := TBitmap.Create;

  Canvas.StretchDraw(Rect(0, 0, Width, Height), CARD_BACK_ENERGY);

  Canvas.Font.Name := 'Early GameBoy';
  Canvas.Font.Size := 16;

  Canvas.Brush.Style := bsClear;

  bitmap.LoadFromFile(AssetsDir + '\' + getimage + '.bmp');
  bitmap.Transparent := false;

  Canvas.StretchDraw(Rect(35, 61, 384, 323), bitmap);

  bitmap.Free;
end;

procedure TCardSprite.RenderAsPokemon;
var
  bitmap: TBitmap;
  cardData, atk: TJSONObject;
  atkEnergy : TJSONArray;
  i, i2: integer;
  b, b2: boolean;
begin
  if not canvas.TryLock then exit;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  bitmap := TBitmap.Create;
  bitmap.Transparent := True;
  bitmap.TransparentColor := clWhite;
  bitmap.TransparentMode := tmFixed;

  Canvas.StretchDraw(Rect(0, 0, Width, Height), CARD_BACK);

  Canvas.Font.Name := 'Early GameBoy';
  Canvas.Font.Size := 16;

  Canvas.Brush.Style := bsClear;

  if Data.Exists('name') then
    Canvas.TextOut(106, 22, Data.get('name').JsonValue.Value);

  Canvas.Font.Name := 'Calibri';
  Canvas.Font.Size := 14;
  Canvas.Font.Style := [fsBold];

  if Data.Exists('data') then
  begin
    cardData := TJSONObject(Data.get('data').JsonValue);
    if cardData <> nil then
    begin
      if cardData.Exists('image') then
      begin
        bitmap.LoadFromFile(AssetsDir + '\' + cardData.get('image')
            .JsonValue.Value + '.bmp');
        bitmap.Transparent := false;

        Canvas.StretchDraw(Rect(35, 61, 384, 323), bitmap);
      end;

      if cardData.Exists('stage') then
      begin
        i := StrToInt(cardData.get('stage').JsonValue.Value);


        if i = 0 then
          Canvas.TextOut(7, 0, 'BASIC')
        else
        begin
          Canvas.TextOut(7, 0, format('STAGE %d', [i]));

          Canvas.Draw(0, 23, CARD_STAGE);

          if cardData.Exists('base-image') then
          begin
            bitmap.LoadFromFile(AssetsDir + '\' + cardData.get('base-image')
                .JsonValue.Value + '.bmp');

            Canvas.StretchDraw(Rect(12, 35, 76, 83), bitmap);
          end;
        end;
      end;

      // Render energy types
      bitmap.Transparent := True;

      if cardData.Exists('weakness') then
      begin
        bitmap.LoadFromResourceName(HInstance, decodeType
            (cardData.get('weakness').JsonValue.Value) + 'Type');
        Canvas.StretchDraw(Rect(41, 505, 66, 530), bitmap);
      end;

      Canvas.Font.Name := 'Early GameBoy';
      Canvas.Font.Size := 16;
      Canvas.Font.Style := [];

      if cardData.Exists('weakness-multiplier') then
        Canvas.TextOut(71, 505,
          cardData.get('weakness-multiplier').JsonValue.Value);

      if cardData.Exists('resistance') then
      begin
        bitmap.LoadFromResourceName(HInstance, decodeType
            (cardData.get('resistance').JsonValue.Value) + 'Type');
        Canvas.StretchDraw(Rect(157, 505, 182, 530), bitmap);
      end;

      if cardData.Exists('resistance-multiplier') then
        Canvas.TextOut(187, 505,
          cardData.get('resistance-multiplier').JsonValue.Value);

      // Render retreat with for loop

      bitmap.LoadFromResourceName(HInstance, 'colorlessType');

      if cardData.Exists('retreat-cost') then
        for i := 1 to StrToInt(cardData.get('retreat-cost').JsonValue.Value) do
          Canvas.StretchDraw(Rect((157 + (28 * (i - 1))), 541,
              ((157 + (28 * (i - 1))) + 25), 566), bitmap);

      // Render hp
      Canvas.Font.Size := 10;

      if cardData.Exists('hp') then
        Canvas.TextOut(351, 4, cardData.get('hp').JsonValue.Value + 'HP');

      if cardData.Exists('type') then
      begin
         bitmap.LoadFromResourceName(HInstance, decodeType
            (cardData.get('type').JsonValue.Value) + 'Type');
        Canvas.StretchDraw(Rect(368, 27, 393, 52), bitmap);
      end;

      // Render Attack1
      if cardData.Exists('attack1') then
      begin
        atk := TJSONObject(cardData.get('attack1').JsonValue);
        if atk <> nil then
        begin
          if atk.Exists('name') then
            Canvas.TextOut(99, 349, atk.get('name').JsonValue.Value);

          if atk.Exists('damage') then
            Canvas.TextOut(336, 365, atk.get('damage').JsonValue.Value);

          b := false;
          b2 := false;
          Canvas.Font.Size := 7;
          if atk.Exists('coin-flip') then
            if uppercase(atk.get('coin-flip').JsonValue.Value) = '1' then
            begin
              b := True;
              b2 := atk.Exists('multiplier');
              if b2 then
                Canvas.TextOut(99, 370, 'Flip a coin, if heads ' +
                  atk.get('multiplier').JsonValue.Value + 'HP');
            end;

          if atk.Exists('effect') then
          begin
            if b then
              if b2 then
                Canvas.TextOut(99, 382,
                  'damage and ' + atk.get('effect')
                    .JsonValue.Value + ' effect')
              else
              begin
                Canvas.TextOut(99, 370, 'Flip a coin, if heads');
                Canvas.TextOut(99, 382, 'apply ' + atk.get('effect').JsonValue.Value +
                    ' effect on enemy');
              end
              else
                Canvas.TextOut(99, 370, 'Causes ' + atk.get('effect').JsonValue.Value +
                    ' effect on enemy');
          end;

          // Render the energy required
          if atk.Exists('energy') then
          begin
            atkEnergy := TJSONArray(atk.Get('energy').JsonValue);
            if atkEnergy <> nil then
            begin
              i2 := atkEnergy.Size - 1;
              if i2 > 3 then i2 := 3;

              for I := 0 to i2 do
              begin

                bitmap.LoadFromResourceName(HInstance, decodeType
                  (atkEnergy.Get(i).Value) + 'Type');

                Canvas.StretchDraw(Rect(
                  31 + (28 * (i - (2 * round((I + 1) * 0.2)))),
                  345 + (28 * round((I + 1) * 0.2)),
                  56 + (28 * (i - (2 * round((I + 1) * 0.2)))),
                  370  + (28 * round((I + 1) * 0.2))), bitmap);
              end;
            end;
          end;

        end;
      end; // End attack 1

      // Render Attack 2
      Canvas.Font.Size := 10;
      if cardData.Exists('attack2') then
      begin
        atk := TJSONObject(cardData.get('attack2').JsonValue);
        if atk <> nil then
        begin
          if atk.Exists('name') then
            Canvas.TextOut(99, 421, atk.get('name').JsonValue.Value);

          if atk.Exists('damage') then
            Canvas.TextOut(336, 437, atk.get('damage').JsonValue.Value);

          b := false;
          b2 := false;
          Canvas.Font.Size := 7;
          if atk.Exists('coin-flip') then
            if uppercase(atk.get('coin-flip').JsonValue.Value) = '1' then
            begin
              b := True;
              b2 := atk.Exists('multiplier');
              if b2 then
                Canvas.TextOut(99, 442, 'Flip a coin, if heads ' +
                  atk.get('multiplier').JsonValue.Value + 'HP');
            end;

          if atk.Exists('effect') then
          begin
            if b then
              if b2 then
                Canvas.TextOut(99, 454,
                  'damage and ' + atk.get('effect')
                    .JsonValue.Value + ' effect')
              else
              begin
                Canvas.TextOut(99, 442, 'Flip a coin, if heads');
                Canvas.TextOut(99, 454, 'apply ' + atk.get('effect').JsonValue.Value +
                    ' effect on enemy');
              end
              else
                Canvas.TextOut(99, 442, 'Causes ' + atk.get('effect').JsonValue.Value +
                    ' effect on enemy');
          end;

          // Render the energy required
          if atk.Exists('energy') then
          begin
            atkEnergy := TJSONArray(atk.Get('energy').JsonValue);
            if atkEnergy <> nil then
            begin
              i2 := atkEnergy.Size - 1;
              if i2 > 3 then i2 := 3;

              for I := 0 to i2 do
              begin

                bitmap.LoadFromResourceName(HInstance, decodeType
                  (atkEnergy.Get(i).Value) + 'Type');

                Canvas.StretchDraw(Rect(
                  31 + (28 * (i - (2 * round((I + 1) * 0.2)))),
                  417 + (28 * round((I + 1) * 0.2)),
                  56 + (28 * (i - (2 * round((I + 1) * 0.2)))),
                  442  + (28 * round((I + 1) * 0.2))), bitmap);
              end;
            end;
          end;

        end;
      end; // End attack 2

    end; // end cardData not nil
  end; // end exists 'cardData'

  bitmap.Free;

  canvas.Unlock;
end;

procedure TCardSprite.RenderAsTrainer;
var
  bitmap : TBitmap;
  cardData : TJSONObject;
  rectangle : TRect;
  text : string;
const
  wordWrap : TTextFormat = [tfWordBreak];
begin
  bitmap := TBitmap.Create;

  bitmap.Transparent := True;
  bitmap.TransparentColor := clWhite;
  bitmap.TransparentMode := tmFixed;

  Canvas.StretchDraw(Rect(0, 0, Width, Height), CARD_BACk_TRAINER);

  Canvas.Brush.Style := bsClear;

  if Data.Exists('name') then
  begin
    Canvas.Font.Name := 'Early GameBoy';
    Canvas.Font.Size := 16;
    Canvas.TextOut(34, 346, Data.get('name').JsonValue.Value);
  end;


  if Data.Exists('data') then
  begin
    cardData := TJSONObject(Data.get('data').JsonValue);
    if cardData <> nil then
    begin
      if cardData.Exists('description') then
      begin
        Canvas.Font.Name := 'Early GameBoy';
        Canvas.Font.Size := 10;
        text := cardData.get('description').JsonValue.Value;
        rectangle := Rect(34, 376, 390, 563);
        Canvas.TextRect(rectangle, text, wordWrap);
      end;

      if cardData.Exists('image') then
      begin
        bitmap.LoadFromFile(AssetsDir + '\' + cardData.get('image')
            .JsonValue.Value + '.bmp');
        bitmap.Transparent := false;
      end;
      Canvas.StretchDraw(Rect(35, 61, 384, 323), bitmap);

    end;
  end;

  bitmap.Free;

end;

procedure TCardSprite.RenderUnknown;
begin

end;

procedure TCardSprite.SetAssetsDir(const Value: String);
begin
  FAssetsDir := Value;
end;

procedure TCardSprite.SetData(const Value: TJSONObject);
var
  hasher : TIdHashSHA1;
begin
  FData := Value;
  // Use to check if state has changed during render cycle.
  hasher := TIdHashSHA1.Create;
  with hasher do
  begin
    hash := HashStringAsHex(FData.ToString);
  end;
  hasher.Free;
  // Automatically render the new data
  Render;
end;

procedure TCardSprite.SetTexture(const Value: GLuint);
begin
  FTexture := Value;
end;

{ TCardModel }

constructor TCardModel.Create;
begin
  ModelType := modelDefault;
  Sprite := TCardSprite.Create;
end;

destructor TCardModel.Destroy;
begin
  Sprite.Destroy;
end;

procedure TCardModel.Draw(RenderType: TCardModelType = modelDefault);
var
  x, y : extended;
begin
  if RenderType = modelActive then
  begin
    y := 2;
    x := 1.425;
  end
  else
  begin
    x := 0.57;
    y := 0.8;
  end;
  glBindTexture(GL_TEXTURE_2D, Sprite.Texture);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0, y, 0.01);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0, 0, 0.01);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(x, 0, 0.01);
    glTexCoord2f(1.0, 1.0);
    glVertex3f(x, y, 0.01);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, MODEL_BACK_TEX);
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0, y, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0, 0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(x, 0, 0);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(x, y, 0);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, MODEL_SIDE_TEX);
  glBegin(GL_QUADS);
    // left
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0, y, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0, 0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0, 0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, y, 0.01);

    // right
    glTexCoord2f(1.0, 1.0);
    glVertex3f(x, y, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(x, 0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(x, 0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(x, y, 0.01);

    // back
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.0, y, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(x, y, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(x, y, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, y, 0.01);

    // front
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.0, 0.0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(x, 0.0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(x, 0.0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, 0.0, 0.01);
  glEnd;
end;

class procedure TCardModel.RenderBackCard;
begin
  glBindTexture(GL_TEXTURE_2D, MODEL_BACK_TEX);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0, 0.8, 0.01);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0, 0, 0.01);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0.57, 0, 0.01);
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.57, 0.8, 0.01);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, MODEL_SIDE_TEX);
  glBegin(GL_QUADS);
    // left
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0, 0.8, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0, 0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0, 0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, 0.8, 0.01);

    // right
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.57, 0.8, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0.57, 0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0.57, 0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.57, 0.8, 0.01);

    // back
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.0, 0.8, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0.57, 0.8, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0.57, 0.8, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, 0.8, 0.01);

    // front
    glTexCoord2f(1.0, 1.0);
    glVertex3f(0.0, 0.0, 0);
    glTexCoord2f(1.0, 0.0);
    glVertex3f(0.57, 0.0, 0);
    glTexCoord2f(0.0, 0.0);
    glVertex3f(0.57, 0.0, 0.01);
    glTexCoord2f(0.0, 1.0);
    glVertex3f(0.0, 0.0, 0.01);
  glEnd;
end;

procedure TCardModel.SetModelType(const Value: TCardModelType);
begin
  FModelType := Value;
end;

procedure TCardModel.SetSprite(const Value: TCardSprite);
begin
  FSprite := Value;
end;

procedure RELOADPKMCARDVAR;
begin
  CARD_BACK := TBitmap.Create;
  CARD_BACK.LoadFromResourceName(HInstance, 'cardBack');
  CARD_BACK.Transparent := True;
  CARD_BACK.TransparentColor := clWhite;
  CARD_BACK.TransparentMode := tmFixed;

  CARD_STAGE := TBitmap.Create;
  CARD_STAGE.LoadFromResourceName(HInstance, 'cardStage');
  CARD_STAGE.Transparent := True;
  CARD_STAGE.TransparentColor := clWhite;
  CARD_STAGE.TransparentMode := tmFixed;

  CARD_BACk_TRAINER := TBitmap.Create;
  CARD_BACk_TRAINER.LoadFromResourceName(HInstance, 'cardBackTrainer');
  CARD_BACk_TRAINER.Transparent := True;
  CARD_BACk_TRAINER.TransparentColor := clWhite;
  CARD_BACk_TRAINER.TransparentMode := tmFixed;

  CARD_BACK_ENERGY := TBitmap.Create;
  CARD_BACK_ENERGY.LoadFromResourceName(HInstance, 'cardBackEnergy');
  CARD_BACK_ENERGY.Transparent := True;
  CARD_BACK_ENERGY.TransparentColor := clWhite;
  CARD_BACK_ENERGY.TransparentMode := tmFixed;
end;

procedure LoadPkmModelTextures;
begin
  LoadTexture('modelBack.bmp', MODEL_BACK_TEX, true);
  LoadTexture('modelSides.bmp', MODEL_SIDE_TEX, true);
end;

end.
