interface ServerInfo {
    "type": String; // gameplay / trade-prep
    "version": String; // server version name scheme LANGUAGE.MAJOR.MINOR.COMMIT(.CUSTOM_NAME) eg. PASCAL.0.1.7f9fe24.PRODUCTION or NODE.0.12.d930200.STABLE
    "online": Number;
    "users": String[]; // Names of users
    "chat": String[]; // All previous chat messages
}

interface ServerPayload {
    "action": String;
    "data": Object;
    "info": ServerInfo;
}