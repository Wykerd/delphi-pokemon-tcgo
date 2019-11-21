unit cardSprite;

interface

uses
  Windows, OpenGL, Graphics, Classes, Math, SysUtils, StrUtils, DBXJSON,
  helpers;

type
  TCardSprite = class(TBitmap)
  private
    FTexture: GLuint;
    FData: TJSONObject;
    FAssetsDir: String;
    procedure SetData(const Value: TJSONObject);
    procedure SetTexture(const Value: GLuint);
    procedure SetAssetsDir(const Value: String);
  published
    constructor Create; override;
    property Texture: GLuint read FTexture write SetTexture;
    property Data: TJSONObject read FData write SetData;
    property AssetsDir: String read FAssetsDir write SetAssetsDir;
    procedure Render;
  public
    class function decodeType(s: string): string;
  end;

implementation

{ TCardSprite }

constructor TCardSprite.Create;
begin
  inherited;
  Height := 590;
  Width := 420;
end;

class function TCardSprite.decodeType(s: string): string;
const
  types: array [0 .. 6] of string = ('water', 'psychic', 'grass', 'fire',
    'fighting', 'electric', 'colorless');
begin
  result := s;
  if not MatchStr(s, types) then
    result := 'colorless';
end;

procedure TCardSprite.Render;
var
  bitmap: TBitmap;
  cardData, atk: TJSONObject;
  atkEnergy : TJSONArray;
  i, i2: integer;
  b, b2: boolean;
begin
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clWhite;
  Canvas.FillRect(Rect(0, 0, Width, Height));

  bitmap := TBitmap.Create;

  // TODO - USE RESOURCE
  bitmap.LoadFromFile('card-back.bmp');
  bitmap.Transparent := True;
  bitmap.TransparentColor := clWhite;
  bitmap.TransparentMode := tmFixed;

  Canvas.StretchDraw(Rect(0, 0, Width, Height), bitmap);

  if Data.Exists('image') then
  begin
    bitmap.LoadFromFile(AssetsDir + '\' + Data.get('image')
        .JsonValue.Value + '.bmp');
    bitmap.Transparent := false;
  end;

  Canvas.StretchDraw(Rect(35, 61, 384, 323), bitmap);

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
      if cardData.Exists('stage') then
      begin
        i := StrToInt(cardData.get('stage').JsonValue.Value);
        if i = 0 then
          Canvas.TextOut(7, 0, 'BASIC')
        else
        begin
          Canvas.TextOut(7, 0, format('STAGE %d', [i]));

          // TODO - USE RESOURCE
          bitmap.LoadFromFile('card-stage.bmp');

          Canvas.Draw(0, 23, bitmap);

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

      // TODO - USE RESOURCE
      if cardData.Exists('weakness') then
      begin
        bitmap.LoadFromFile(AssetsDir + '\' + decodeType
            (cardData.get('weakness').JsonValue.Value) + '.bmp');
        Canvas.StretchDraw(Rect(41, 505, 66, 530), bitmap);
      end;

      Canvas.Font.Name := 'Early GameBoy';
      Canvas.Font.Size := 16;
      Canvas.Font.Style := [];

      if cardData.Exists('weakness-multiplier') then
        Canvas.TextOut(71, 505,
          cardData.get('weakness-multiplier').JsonValue.Value);

      // TODO - USE RESOURCE
      if cardData.Exists('resistance') then
      begin
        bitmap.LoadFromFile(AssetsDir + '\' + decodeType
            (cardData.get('resistance').JsonValue.Value) + '.bmp');
        Canvas.StretchDraw(Rect(157, 505, 182, 530), bitmap);
      end;

      if cardData.Exists('resistance-multiplier') then
        Canvas.TextOut(187, 505,
          cardData.get('resistance-multiplier').JsonValue.Value);

      // Render retreat with for loop

      // TODO - USE RESOURCE
      bitmap.LoadFromFile('colorless.bmp');

      if cardData.Exists('retreat-cost') then
        for i := 1 to StrToInt(cardData.get('retreat-cost').JsonValue.Value) do
          Canvas.StretchDraw(Rect((157 + (28 * (i - 1))), 541,
              ((157 + (28 * (i - 1))) + 25), 566), bitmap);

      // Render hp
      Canvas.Font.Size := 10;

      if cardData.Exists('hp') then
        Canvas.TextOut(351, 4, cardData.get('hp').JsonValue.Value + 'HP');

      // TODO - USE RESOURCE
      if cardData.Exists('type') then
      begin
        bitmap.LoadFromFile(AssetsDir + '\' + decodeType
            (cardData.get('type').JsonValue.Value) + '.bmp');
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
                

                // TODO - USE RESOURCE
                bitmap.LoadFromFile(AssetsDir + '\' +
                  decodeType(atkEnergy.Get(i).Value) + '.bmp');

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


                // TODO - USE RESOURCE
                bitmap.LoadFromFile(AssetsDir + '\' +
                  decodeType(atkEnergy.Get(i).Value) + '.bmp');

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
end;

procedure TCardSprite.SetAssetsDir(const Value: String);
begin
  FAssetsDir := Value;
end;

procedure TCardSprite.SetData(const Value: TJSONObject);
begin
  FData := Value;
end;

procedure TCardSprite.SetTexture(const Value: GLuint);
begin
  FTexture := Value;
end;

end.
