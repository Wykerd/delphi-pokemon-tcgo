import React, { useContext, useEffect, useState } from "react";
import { Switch, Route, Link } from 'react-router-dom';
import Page from './Page';
import { DecksDataContext } from './context/UserDataContexts';
import FirebaseContext from './context/FirebaseContext';
import UserContext from './context/UserContext';
import './css/deck.css';

export default function Decks () {
    return <Page>
        <h1>Manage Decks</h1>
        <Switch>
            <Route exact path="/decks" component={DecksList} />
            <Route exact path="/decks/:id" component={ManageDeck} />
        </Switch>
    </Page>
}

function ManageDeck ({ match }) {
    const firebase = useContext(FirebaseContext);
    const user = useContext(UserContext);
    const [ data, setData ] = useState({name: 'Loading...', cards: [], card_sorted: {}});
    // const [ stateChanged, setStateChanged ] = useState(false);

    useEffect(()=>{
        if (!user || !firebase) return;
        firebase.firestore().collection('profiles').doc(user.uid).collection('decks').doc(match.params.id).get().then(doc=>{
            let state = {};
            
            if (!doc.data().cards) return;
            doc.data().cards.forEach(el=>{
                if (!state.hasOwnProperty(el)) state[el] = 0;
                state[el]++;
            });

            setData({...doc.data(), _id: doc.id, card_sorted: state});
        });
    }, [firebase, user]);

    return <>
        <h2>Build Deck</h2>
        <input type="text" value={data.name} className="deck-name" />
        <ul>
            {   
                //data.cards.map((el, i)=><Card key={i} cardId={el} />)
                Object.keys(data.card_sorted).map(el=><Card key={el} cardId={el} occurance={data.card_sorted[el]} />)   
            }
        </ul>
    </>
}

function Card ({ cardId, occurance }) {
    const firebase = useContext(FirebaseContext);
    const [ name, setName ] = useState('Loading...');

    useEffect(()=>{
        firebase.firestore().collection('cards').doc(cardId.toString()).get().then(doc=>{
            if (!doc.data().name) return;

            setName(doc.data().name);
        })
    }, [firebase, cardId]);

    return <li>
        <div>{`${occurance}x`}</div>
        <div>{name}</div>
    </li>;
}

function DecksList () {
    const decks = useContext(DecksDataContext);

    return <ul className="decks">
        {
            decks.map(el=><li key={el._id}>
                <Link className="log-item" to={`/decks/${el._id}`}>{el.name}</Link>
            </li>)
        }
    </ul>;
}