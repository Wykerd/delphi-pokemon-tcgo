// Copyright 2019 Daniel Wykerd
//
// versions Unit
//
// Author:      Daniel Wykerd
// Year:        2019
//
// Function:    Contains all the version contants for the server and/or client.
//              Spesifies the versions of the client, server and API the source
//              is compatible with.
//
// Description: The server / client uses the constants to ensure compatibility
//              with the other components (client, server, REST API)

unit versions;

interface

const
  // SERVER VERSION CONSTANTS //

    // VERSION NUMBERS
      // Which version of the spec does the server comply with?
      // Semantic versioning.
      // If the server is modded and the JSON packages no longer conform to the spec,
      // append some identifier to the end to avoid conflict with the official spec.
      // For example use '1-creative_mod' instead of '10' as the client might
      // try to use version 10 of the official API.
      SRV_API_VERSION = '1';
      SRV_CLIENT_VERSION = '0.1.0';
      SRV_SERVER_VERSION = '0.1.0';
      // Does the server support older clients?
      SRV_LEGACY_SUPPORT = 'true';
      // Up to what client is it compatible?
      SRV_BACKWARDS_COMPATIBILITY = '0.0.0';
    // END VERSION NUMBERS //

    // SERVER INFO //
    // Q: Why is server info required?
    // A: Due to the nature of the program, other developers can create their own servers
    //    as long as the JSON packages are compatible with the client. Thus the info below
    //    is used to identify the server to the client and API.

      // CUSTOM NAME
        // The custom name can be set to anything. It is a human readable identifier for the server.
        // Keep it short.
      SRV_CUSTOM_NAME = 'Vanilla';

      // The info below is only used for datacollection by the API
        // If the game has public repo put the link here for other devs to contribute
        // Otherwise leave as 'undefined;
        SRV_GIT_REPO = 'https://github.com/Wykerd/card-game-pat.git';
        //
        SRV_WEBSITE = 'pokemon.wykerd.io';
      // END API DATA COLLECTION INFO //
    // END SERVER INFO //

  // END SERVER CONSTANTS //

  // CLIENT CONSTANTS //
    // VERSION NUMBERS
      // No api version as the client doesnt directly communicate with the REST API
      CLIENT_SERVER_VERSION = '0.1.0';
      CLIENT_APP_VERSION = '0.1.0';
    // END VERSION NUMBERS //
  // END CLIENT CONSTANTS //

  // PROGRAM CONSTANTS //
  // PROGRAM VERSION NAME SCHEME
    // ENGINE_LANGUAGE.MAJOR.MINOR.PATCH(-PRE_RELEASE.COMMIT)
    // ENGINE_LANGUAGE = main language used in the source.
    // PRE_RELEASE types = beta, alpha, indev, rc (beta that could be final)
    // Non production builds eg. PASCAL.0.1.0-beta.7f9fe24 or NODE.0.12.0-rc.d930200
    // Production builds eg. PASCAL.0.1.12 or NODE.1.2.0
  VERSION_NAME = 'PASCAL.0.1.0-indev.de45a92';

implementation

end.
