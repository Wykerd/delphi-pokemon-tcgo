/* 
Animatable properties
prize-card -> hand (card)
benched-card -> hand (retreat)
benched-card -> discard (energy)
active-card -> discard (K.O. or retreat)
card.attached-energy -> discard (this goes with retreat cost)
hand -> discard (trainer)
hand -> bench (pokemon)(energy)
hand -> active (energy)
*/

interface ClientAnimation {
    "target": String; // player or oponent data
    "property": String; // Property of ClientPlayerState or ClientOponentState
    "style": String; // direct or centre-halt (display card in centre first, for trainers or K.O.)
    "index"?: Number; // used if property to animate is array 
}

interface ClientPlayerState {
    "prize-cards": Card[];
    "benched-cards": Card[];
    "deck": Card; // The top card of the deck!
    "active-card": Card;
    "hand": Card[];
    "discard": Card[]; // last 2 discards
}

interface ClientOponentState {
    "prize-cards": Number;// can't see prizes
    "benched-cards": Card[];
    "active-card": Card;
    "hand": Number; // can't see hand
    "discard": Card[]; // last 2
}

interface GameplayState {
    "turn": String; // oponent or player
    "stage": String;
    // init, card_retreat+energy, attack
}

interface ClientState {
    "animation"?: ClientAnimation[]; // Complete animations in order of array;
    "player"?: ClientPlayerState;
    "opponent"?: ClientOponentState;
    "gameplay"?: GameplayState;
}