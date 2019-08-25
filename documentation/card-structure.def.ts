interface CardSet {
    "id": String;
    "name": String;
    "quantity": Number;
}

interface AttackData {
    "id": String;
    "name": String;
    "energy": String[];
    "damage": Number;
    "effect"?: String;
    "multiplier"?: String;
    "coin-flip": String;
}

interface PokemonData {
    "id": String;
    "type": String;
    "resistance": String;
    "weakness": String;
    "resistance-multiplier": String;
    "weakness-multiplier": String;
    "retreat-cost": Number;
    "attack1": AttackData;
    "attack2": AttackData;
    "base"?: String; // ID only
    "base-image"?: String; // Image of base
    "stage": Number;
    "hp": Number;
    "gx": Boolean;
    "attached-stages"?: Card[];
}

interface TrainerData {

}

interface Card {
    "id": String;
    "type": String;
    "image": String;
    "set": CardSet;
    "set-number": Number;
    "rarity": Number;
    "name": String;
    "data"?: PokemonData | TrainerData;
    "attached-energy"?: String[];
}