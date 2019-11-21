# Problem
Design and develop a card game for multiple players to play at the same time (multiplayer game). All functions should fit in one logically related program.

# Requirements
- The project requires to make use of a database with at least two tables to programmatically perform CRUD operations and calculations using multiple tables. 
- Text files should also be used to populate data structures. 
- The GUI should consist of at least three forms that should be navigable by the user. 
- At least one other data structure should be used along with the database. 

# Solution
For this project, it was desided that a trading card game would best meet the requirements of the project and the game Pokemon TCG was chosen as the game that will be made.
To allow for multiplayer gameplay the program should allow users to create their own servers.
Thus the project and final project will consist of two parts, the server and client.

## The Game
Pokemon Trading Card Game is a collectible card game, based on Nintendo's Pokémon franchise.

Each player has a deck of 60 cards.

The players draw the first 7 cards, and then set aside the top 6 cards as prize cards.

Each player places one basic pokemon as their active pokemon and bench up to 5 others.

A coin is then fliped to determine who starts.

\* Pokemon is abbreviated to pkm

Each turn consist of the following steps:
1. Draw a card
2. Do any of the following actions in any order
    - Bench basic pkm
    - Evolve benched or active pkm
    - Attach energy to benched or active pkm
    - Play trainer card
    - Retreat (once)
3. Attach and end turn.
4. If you K.O. your oponent's active card, you draw one of your prize cards.

The game is won by the following criteria:
1. Taking all your prize cards.
2. K.O. all your oponents in-game pkm (benched and active)
3. If your oponent has no cards in their deck at the begining of their turn

## The Server
The server does most of the processing during a game and handles all the database processing. 

The server should handle multiple game instances simultaneously if possible, but this feature isn't required.

Functions of the server:

### Fetch data from Authentication and Users server
Make a TCP connection to the main Authentication and Users server to gather user data and ensure that cards are synced between servers.

1. Ensure the user ID exists
2. Get the card id's of the user
3. Inform the server of card trades

### Database operations
1. Manage ```Users``` table
    - Create records of new users
    - Update existing users

2. Manage ```Cards``` table
    - Read card data to use in-game

3. Manage ```Matches``` table
    - Create new matches
    - Update match data
        - Scores
        - Rounds
        - etc.
    - Read match data to calculate user scores and statistics
    - Don't allow match deletion as it is a perminant record

### Session management
The server should handle client sessions. 

It should send data to the clients regarding the current state of the game. 

The data will be sent as a JSON payload to the client to interpret. 

The server should also ensure that the client is authenticated with a Unique ID to ensure that hacked clients can't act (send payloads to the server) on behalf of another client.

The servers should also be able to handle payloads from the client regarding actions that the player has performed.

### Game management
1. Store the current game state in the appropriate data structures.

2. Update the game state when clients perform actions in-game.

3. Notify all the client of the change in state.

4. Update the database

5. Determine the winners

## The Client
The client provides the GUI for the users to interact with the server.

### Manage UID for authentication
- Generate the UID
- Store the UID in a text file
- Authenticate payloads with the UID.

### Rendering
Render the game in the state last received from the server.

### Notifying
Notify the server of any changes made to the game state.

This includes:
- Taking your turn in the game
- Trading cards with users
- Chatting

### Fetching
Fetch data from the server by making a request via JSON payload to the server.

This includes:
- Server info
- User info
- Game state

### Storing server list
Store the server list in a JSON formatted text file. Use the file to populate a TJSONArray object and display information in list on server list UI.