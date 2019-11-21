interface ServerInfo {
    "online": Number;
    "users": String[]; // Names of users
    "motd": String;
    "name": String;
}

interface PreflightInfo {
    "version": String;
    "legacy-support": Boolean;
    "backwards-version"
}

interface GameQueue {
    "status": String; // waiting_for_players
}

interface GameStart {
    "start": String; // "you" or "oponent" string saying who wil start!
}

interface ServerPayload {
    "action": String;
    "data": Object;
}
/*
reasons:
- session_exists
- user_undefined
- no_decks
- internal_error
- api_error
- server_not_registered
*/

interface ClientAuthenticate {
    "success": Boolean;
    "reason"?: String; // if not authenticated
    // if authenticated
    "sid"?: String; // GUID
    "decks"?: String[]; // Deck names
}

interface ClientGameUseDeck {
    index: Number;
}

interface ServerConfirming {
    sucess: Boolean; 
    // if not success
    reason?: String; 
    // game-use-state -> out_of_range, deck_build_failed, invalid_request
    // game-ready -> no_deck_used
}