# Problem
Design and develop a card game for multiple players to play at the same time (multiplayer game). All functions should fit in one logically related program.

# Requirements
- The project requires to make use of a database with at least two tables to programmatically perform CRUD operations and calculations using multiple tables. 
- Text files should also be used to populate data structures. 
- The GUI should consist of at least three forms that should be navigable by the user. 
- At least one other data structure should be used along with the database. 

# Solution
To allow for multiplayer gameplay the program should allow users to create their own servers.
Thus the project and final project will consist of two parts, the server and client.

## The Server
The server does most of the processing during a game and handles all the database processing. 

The server should handle multiple game instances simultaneously if possible, but this feature isn't required.

Functions of the server:

### Database operations
1. Manage ```Users``` table
    - Create records of new users
    - Update existing users

2. Manage ```Cards``` table
    - Create one-to-many relationships between cards and users
    - Update or delete references between cards and users in the case users trade cards
    - Read card data, this includes attacks and attack data from the attacks table

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
Store the server list in a JSON formatted text file.