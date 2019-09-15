# User Requirments
The user is the target audience and will thus determine the needs and requirements of the program. Determine the clients/users and their requirements.

The aim is to identify the users, user needs, acceptable limitations and processing requirements of the system. Use a table or a use case diagram to explain the role, activity and limitations of each user of the system.

## User Types
There are two use types in the gameplay server network
- Players
- Host

There are many players connected to a server host, the host has direct access to manipulate user data and card data, while the players does not have direct access to the data.

## User Description
|| Player | Host |
| --- | --- | --- |
| Data access | The player does not have direct access to the game's database and only gets restricted snippets of data sent to it when it is requird. This ensures that the player cannot cheat by using a modded client as the server only sends the relevant data instead of all data. For example, only the player's hand is sent to the client, so the player doesn't have access to the other user's hand and can thus not see it | The host has full uncontrolled access to the database and is thus expected to maintain it using the server UI, by keeping the card data up to date and handle banned players. |
| Processing | The player clients receive the data snippets in JSON packets from the host's server and processes it to render the current game state to the UI | The host's machine is responsible for processing all game logic through algorthms and additionally handling all currently connected player user sessions. The host can also join a game and then also has to also perform all the processing done by players as well |
| Additional Limitations | No additional limitations. | While the host is in a game and it acts as a player and thus loses all the privelages of being host for the duration of the game and is effectively a player user. 
