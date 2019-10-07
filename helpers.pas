unit helpers;

interface

uses
  Classes, System.JSON, SysUtils, IdSync, ComCtrls, Character, Variants,
  Textures, OpenGL, System.RTLConsts,
  System.Generics.Defaults, System.Generics.Collections;

type
  // TArray<t> = array of t;

  TJsonObjectHelper = class helper for TJsonObject
  public
    // Helper to find a JSON pair based on the pair string part
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    function Exists(const PairName: UnicodeString): boolean;
    procedure ExistCall(const PairName: UnicodeString;
      callback: TProc<TJSONPair>);
  end;
  // //

  // Events //
  TGenericEvent<T1> = procedure(A: T1) of object;
  TGenericEvent = procedure of object;
  // For older support before renaming!
  TObjProcedure = TGenericEvent;

  // Code pulled from StackOverflow by user who copied it from an newer version
  // of delphi as this version doesn't include this class
  TAnonymousThread = class(TThread)
  private
    FProc: TProc;
  protected
    procedure Execute; override;
  public
    constructor Create(const AProc: TProc);
  end;

const
  StaticRandom_1: array [0 .. 59] of Extended = (0.1328927488938767,
    0.0072858930858721305, 0.19895602074193564, 0.8677014024324408,
    0.06635165037636392, 0.3686947304511705, 0.8446796641845158,
    0.1116629367672326, 0.8649280191213113, 0.02760657991032822,
    0.27345140114324606, 0.696177715045023, 0.22944688782855205,
    0.07841248827119074, 0.029722471350650315, 0.5788432991360668,
    0.5613135229914448, 0.45301394555571073, 0.47337359441368565,
    0.4669126855736203, 0.7611864565216075, 0.8576494971425399,
    0.07633042840757143, 0.2867155677574529, 0.06637040249528692,
    0.9159944895166505, 0.1163339499822289, 0.6483284618265359,
    0.5710219340962157, 0.800712344785586, 0.027329009102930124,
    0.5854874103799188, 0.5711219316159317, 0.5400911036793332,
    0.9608907613629103, 0.10045318586519092, 0.687942667090905,
    0.2632443806103406, 0.06908379626469952, 0.10075381911823822,
    0.24774303614761828, 0.4232091060556171, 0.6924459796854501,
    0.26905841843495715, 0.44689399365415183, 0.3648843075783623,
    0.6649476910038121, 0.26522716354266995, 0.8035032918814893,
    0.0017038053927587171, 0.2084299291127698, 0.39365509988157643,
    0.22680919410276568, 0.5958233803129402, 0.5471899726021434,
    0.591502847149888, 0.7665519561936482, 0.8350092799283158,
    0.09333491609743616, 0.7503058920620593);

  StaticRandom_2: array [0 .. 59] of Extended = (0.33496388431607516,
    0.5908188308417794, 0.7551275508880979, 0.30667806832515554,
    0.14011069203197168, 0.601493114910185, 0.6721516951061492,
    0.052821424056669786, 0.29681669660916477, 0.5583959559638962,
    0.4300477539165013, 0.012120695459227226, 0.6607125718902047,
    0.8918521769485013, 0.18473844334809053, 0.7993885314785403,
    0.4851088076245005, 0.21688086123586747, 0.3176015741117184,
    0.696776250646578, 0.15804037982274832, 0.3955727812985177,
    0.6471469293362107, 0.573998847101143, 0.301783499357555,
    0.9909525076496368, 0.1305993280840585, 0.9965325760954988,
    0.7834893571573096, 0.7007631682411994, 0.4814245704500091,
    0.2888515599367609, 0.3846015113989467, 0.6760069758343934,
    0.9307769691749035, 0.8070057099073564, 0.9233200310521326,
    0.9983353453492922, 0.6531448014868784, 0.7295091014378798,
    0.27025763357767674, 0.6497949779550831, 0.17320439923788933,
    0.28792652661310303, 0.33482434616175705, 0.7628536072021481,
    0.2588126457932505, 0.5885776391488851, 0.16812443728636706,
    0.4833932845372324, 0.489194652790377, 0.7248003180301761,
    0.5369588498745073, 0.538180914351875, 0.6854195221235158,
    0.14028656731602007, 0.6990765623788591, 0.6623815561449833,
    0.816846226566895, 0.36833586114242567);

  // String manipulation functions
function strJoin(const arr: array of string; const delim: string): string;
function strSplit(s: string; delimiter: string): TArray<string>;

// JSON LOADER
function LoadJSONFromFile(s: string): TJsonObject;

// More JSON helpers from https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
function StripNonJson(s: string): string;

// Check if a variant is undefined. Used to check for empty Text fields in db.
function UndefinedVar(v: Variant): boolean;
procedure glImgWrite(strText: string);

implementation

procedure glImgWrite(strText: string);
var
  I, intAsciiCode: integer;
  imgcharWidth: GLfloat;
  imgcharPosX: GLfloat;
begin

  imgcharWidth := 1.0 / 66;
  strText := UpperCase(strText);

  for I := 1 to length(strText) do
  begin

    if ord(strText[I]) > 31 then // only handle 66 chars
    begin
      intAsciiCode := ord(strText[I]) - 32;
      imgcharPosX := length(strText) / 2 * 0.08 - length(strText) * 0.08 +
        (I - 1) * 0.08;
      // Find the character position from the origin [0.0 , 0.0 , 0.0]  to center the text
      glBegin(GL_QUADS);

      glTexCoord2f(imgcharWidth * intAsciiCode, 0.0);
      glVertex3f(-0.04 + imgcharPosX, -0.04, 0.0);

      glTexCoord2f(imgcharWidth * intAsciiCode + imgcharWidth, 0.0);
      glVertex3f(0.04 + imgcharPosX, -0.04, 0.0);

      glTexCoord2f(imgcharWidth * intAsciiCode + imgcharWidth, 1.0);
      glVertex3f(0.04 + imgcharPosX, 0.04, 0.0);

      glTexCoord2f(imgcharWidth * intAsciiCode, 1.0);
      glVertex3f(-0.04 + imgcharPosX, 0.04, 0.0);
      glEnd;
    end;
  end;
end;

function UndefinedVar(v: Variant): boolean;
begin
  result := false;
  result := VarIsClear(v) or VarIsEmpty(v) or VarIsNull(v);
end;

function LoadJSONFromFile(s: string): TJsonObject;
var
  tF: textfile;
  data, tmp: string;

begin
  // Init
  data := '';
  tmp := '';

  if not fileExists(s) then
  begin
    result := nil;
    exit;
  end;

  AssignFile(tF, s);
  try
    reset(tF);
    while not eof(tF) do
    begin
      readln(tF, tmp);
      data := data + tmp;
    end;

    closefile(tF);

    // Parse the JSON
    result := TJsonObject(TJsonObject.ParseJSONValue
      (TEncoding.ASCII.GetBytes(StripNonJson(data)), 0));
  except
    result := nil;
  end;

end;

// Created to act as javascript string helpers
function strJoin(const arr: array of string; const delim: string): string;
var
  I: integer;
begin
  result := '';
  for I := 0 to length(arr) - 2 do
    result := result + arr[I] + delim;
  result := result + arr[length(arr) - 1];
end;

// Created to act as javascript string helpers
function strSplit(s: string; delimiter: string): TArray<string>;
var
  iP: integer;
  remain: string;
  found: boolean;
begin
  found := true;
  remain := copy(s, 1, MaxInt);
  SetLength(result, 0);

  while found do
  begin
    iP := Pos(delimiter, remain);
    if iP > 0 then
    begin
      SetLength(result, length(result) + 1);
      result[High(result)] := copy(remain, 1, iP - 1);
      Delete(remain, 1, iP);
    end
    else
    begin
      found := false;
      SetLength(result, length(result) + 1);
      result[High(result)] := remain;
    end;
  end;

end;

{ TJsonObjectHelper }

procedure TJsonObjectHelper.ExistCall(const PairName: UnicodeString;
  callback: TProc<TJSONPair>);
var
  pair: TJSONPair;
begin
  // Check to see if the pair exists

  // This might be a bit slow due to it being a linear search... For a lot
  // usage a binary search might be better - DWykerd
  pair := Get(PairName);

  if pair <> nil then
    if pair.JsonValue <> nil then
      callback(pair);
end;

function TJsonObjectHelper.Exists(const PairName: UnicodeString): boolean;
var
  pair: TJSONPair;
begin
  // Check to see if the pair exists
  result := false;

  // This might be a bit slow due to it being a linear search... For a lot
  // usage a binary search might be better - DWykerd
  pair := Get(PairName);

  if pair <> nil then
    if pair.JsonValue <> nil then
      result := true;
end;

function TJsonObjectHelper.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: integer;
begin
  // This helper has been copied from github
  // CREDITS: https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
  for I := 0 to Size - 1 do
  begin
    Candidate := Get(I);
    if (Candidate.JsonString.Value = PairName) then
      exit(Candidate);
  end;
  result := nil;
end;

{ TAnonymousThread }

constructor TAnonymousThread.Create(const AProc: TProc);
begin
  inherited Create(true);
  FreeOnTerminate := true;
  FProc := AProc;
end;

procedure TAnonymousThread.Execute;
begin
  FProc();
end;

// Remove whitespaces http://edn.embarcadero.com/article/40882
// CREDITS: https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
function StripNonJson(s: string): string;
var
  ch: char;
  inString: boolean;
begin
  result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;
    if TCharacter.IsWhiteSpace(ch) and not inString then
      continue;
    result := result + ch;
  end;
end;

end.
