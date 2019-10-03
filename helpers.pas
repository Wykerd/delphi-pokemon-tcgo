unit helpers;

interface

uses
  Classes, System.JSON, SysUtils, IdSync, ComCtrls, Character, Variants;

type
  //TArray<t> = array of t;
  TStrArr = array of string;

  TJsonObjectHelper = class helper for TJsonObject
  public
    //Helper to find a JSON pair based on the pair string part
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    function Exists (const PairName: UnicodeString): boolean;
    procedure ExistCall (const PairName: UnicodeString; callback: TProc<TJSONPair>);
  end;
  // //

  // Events //
  TGenericEvent<T1> = procedure (A: T1) of object;
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

// String manipulation functions
function strJoin (const arr : array of string; const delim: string): string;
function strSplit (s: string; delimiter: string): TStrArr;

// JSON LOADER
function LoadJSONFromFile (s: string) : TJSONObject;

// More JSON helpers from https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
function StripNonJson(s: string): string;

// Check if a variant is undefined. Used to check for empty Text fields in db.
function UndefinedVar(v: Variant): boolean;

implementation

function UndefinedVar(v: Variant): boolean;
begin
  result := false;
  result := VarIsClear(v) or VarIsEmpty(v) or VarIsNull(v);
end;

function LoadJSONFromFile (s: string) : TJSONObject;
var
  tF: textfile;
  data, tmp : string;

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

    closefile(tf);

    // Parse the JSON
    result := TJsonObject(TJSONObject.ParseJSONValue(TEncoding.ASCII.GetBytes(StripNonJson(data)),0));
  except
    result := nil;
  end;

end;

// Created to act as javascript string helpers
function strJoin (const arr : array of string; const delim: string): string;
var
  I: Integer;
begin
  result := '';
  for I := 0 to length(arr) - 2 do
    result := result + arr[i] + delim;
  result := result + arr[length(arr)-1];
end;


// Created to act as javascript string helpers
function strSplit(s: string; delimiter: string) : TStrArr;
var
  iP : integer;
  remain: string;
  found: boolean;
begin
  found := true;
  remain := copy(s,1,MaxInt);
  SetLength(result, 0);

  while found do
  begin
    iP := Pos(delimiter, remain);
    if iP > 0 then
    begin
      SetLength(result, length(result) + 1);
      result[High(result)] := Copy(remain, 1, iP - 1);
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
  pair : TJSONPair;
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
  pair : TJSONPair;
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
  I: Integer;
begin
  // This helper has been copied from github
  // CREDITS: https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
  for i := 0 to Size - 1 do
  begin
    Candidate := Get(i);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
  end;
  Result := nil;
end;

{ TAnonymousThread }

constructor TAnonymousThread.Create(const AProc: TProc);
begin
  inherited Create(True);
  FreeOnTerminate := True;
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
  Result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;
    if TCharacter.IsWhiteSpace(ch) and not inString then
      continue;
    Result := Result + ch;
  end;
end;

end.
