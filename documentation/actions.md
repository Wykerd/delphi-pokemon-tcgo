# Server to Client Actions (server-to-client-payloads.def)
- authenticate (server-to-client-payloads.def/ClientAuthenticate) (implimented) (implimented)
- chat () (implimented) (implimented)
- join () (implimented) (implimented)
- info (server-to-client-payloads.def) () ()
- error () () ()
- game-start (client-state.def) (partial_impimentation) () ()
- game-queue (server-to-client-payloads.def) (implimented) () ()
- game-state (client-state.def) () ()
- game-end () () ()
- game-quit () () ()
#### To Add Later
- disconnect () () ()
- game-premature-end () () ()

# Client to Server Actions
- authenticate () () () // authenticating will cause you to join the game
- info (server-to-client-payloads.def) () ()
- chat () (implimented) (implimented - no render)
- game-ready () () ()
- game-use-deck (server-to-client-payloads.def) (implimented) (implimented)
- game-action () () ()
- game-quit () () ()