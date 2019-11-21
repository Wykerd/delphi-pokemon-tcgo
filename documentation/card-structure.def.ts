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
    "resistance"?: String;
    "weakness"?: String;
    "resistance-multiplier"?: String;
    "weakness-multiplier"?: String;
    "retreat-cost": Number;
    "attack1": AttackData;
    "attack2"?: AttackData;
    "base"?: String; // ID only
    "base-image"?: String; // Image of base
    "stage": Number; // 0 = base
    "hp": Number;
    "image": String;
    "attached-stage"?: Card;
}

interface TrainerData {
    "id": String;
    "image": String;
    "function": String;
    "description": String;
}

interface Card {
    "id": String;
    "type": String; // pokemon, trainer, energy
    "set": CardSet;
    "set-number": Number;
    "rarity": Number; // 1 - common, 2 - uncommon, 3 - rare
    "name": String;
    "data"?: PokemonData | TrainerData;
    "attached-energy"?: String[];
}