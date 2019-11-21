# pokemon-tcgo-pascal
A simple Pokemon TCG game made in Pascal

# NOTE:
- This was done as a school project and was not intended to be deployed into production. 
- I will not be working on this project for a while, but contributions are welcome. 
- The game is under development and is very unstable.

# Introduction
This is a simple 3d Pokemon Trading Card Game implimentation using OpenGL.

# Features
## Implimented
### Serverside
All basic server functionallity is implimented within the server class. This includes but is not limited to:
1. Authentication
2. Session Management
3. Game state generation from the data in Firebase Firestore and MS Access database
4. JSON package protocol. Read more at sesion "Server and Client Communciation"
5. Game Logic for Pokemon and Energy cards.
6. Basic card data.
7. In-game chat.
8. **Functional gameplay.** Read more about how this works in section "Gameplay"

### Client
1. Basic Views
    - Start (VCL)
    - Server Listings (VCL)
    - Game lobby / Pregame (VCL)
    - Error (VCL)
    - Game (OpenGL)
2. Server connection
3. Server lisiting caching
4. Deck selection
5. Render gameplay state from the server
6. Unstable, but **functional gameplay**. Read more at "Gamplay"

### Trading PWA
1. View server access logs
2. View available decks
3. Add friends.
4. Update profile
5. Profile image upload to Firebase Cloud Storage.

### Firebase Cloudfunctions API
1. Server registration
2. GET user deck data

## Backlog
Text in bracket indicates where the feature is to be implimented.
1. In-game chat (server)
2. Trading cards with other players (Firebase function)
3. Booster pack winning (Firebase function)
4. Trading UI Styling (React PWA)

## Coming Soon
See the todo features section

# TODO
This game is under development and is far from a stable release.

## Things that require immediate work:
1. Card rendering. The cards textures has to be locally cached instead of being generated each time the game is launched. This is the main source of the instablity of the software.
2. Thread management requires some optimisations.

# THIS DOCUMENTATION IS INCOMPLETE AND WILL BE FINISHED LATER
