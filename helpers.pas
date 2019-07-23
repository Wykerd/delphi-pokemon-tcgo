unit helpers;

interface

uses
  Classes, DBXJSON;

type
  TStrArr = array of string;

  // TJSONObjectHelper has been copied from github
  // CREDITS: https://gist.github.com/fabriciocolombo/4236ce010787d86b5c65
  TJsonObjectHelper = class helper for TJsonObject
  public
    //Helper to find a JSON pair based on the pair string part
    function Get(const PairName: UnicodeString): TJSONPair; overload;
  end;

// String manipulation functions
function strJoin (const arr : array of string; const delim: string): string;
function strSplit (s: string; delimiter: string): TStrArr;

implementation

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

function TJsonObjectHelper.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do
  begin
    Candidate := Get(i);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
  end;
  Result := nil;
end;

end.
