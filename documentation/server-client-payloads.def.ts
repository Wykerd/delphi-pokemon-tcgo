interface ServerInfo {
    "type": String; // gameplay / trade-prep
    "version": String; // server version name scheme LANGUAGE.MAJOR.MINOR.COMMIT(.CUSTOM_NAME) eg. PASCAL.0.1.7f9fe24.PRODUCTION or NODE.0.12.d930200.STABLE
    "online": Number;
    "users": String[]; // Names of users
    "motd": String;
    "name": String;
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
*/

interface ClientAuthenticate {
    "success": Boolean;
    "reason"?: String; // if not authenticated
    // if authenticated
    "sid"?: String; // GUID
    "decks"?: String[]; // Deck names
}