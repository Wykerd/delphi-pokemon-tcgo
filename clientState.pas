unit clientState;

interface

function DecodeAuthReason (s: string) : string;

implementation

// General //
function DecodeAuthReason (s: string) : string;
begin
  result := 'Unknown reason. Is the server modded? Make sure it complies with the spec';
  if s = 'session_exists' then result := 'There is already a session issued to this user'
  else if s = 'user_undefined' then result := 'You are not registered with the API'
  else if s = 'no_decks' then result := 'You have not made any decks in the API'
  else if s = 'internal_error' then result := 'Internal server error'
  else if s = 'server_not_registered' then result := 'Server not registered with API'
  else result := 'Unknown - server not to spec';
end;

end.
