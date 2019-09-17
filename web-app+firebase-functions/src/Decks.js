import React, { useContext, useEffect, useState } from "react";
import { Switch, Route, Link } from 'react-router-dom';
import Page from './Page';
import { DecksDataContext } from './context/UserDataContexts';
import FirebaseContext from './context/FirebaseContext';
import UserContext from './context/UserContext';

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
    const [ data, setData ] = useState({name: 'Loading...', cards: []});
    const [ stateChanged, setStateChanged ] = useState(false);

    useEffect(()=>{
        if (!user || !firebase) return;
        firebase.firestore().collection('profiles').doc(user.uid).collection('decks').doc(match.params.id).get().then(doc=>{
            setData({...doc.data(), _id: doc.id});
        });
    }, [firebase, user]);

    return <>
        <h2>Build Deck</h2>
        <input type="text" value={data.name} className="deck-name" />
        <ul>
            {
                data.cards.map((el, i)=><Card key={i} cardId={el} />)
            }
        </ul>
    </>
}

function Card ({ cardId }) {
    const firebase = useContext(FirebaseContext);
    const [ name, setName ] = useState('Loading...');

    useEffect(()=>{
        firebase.firestore().collection('cards').doc(cardId.toString()).get().then(doc=>{
            if (!doc.data().name) return;

            setName(doc.data().name);
        })
    }, [firebase, cardId]);

    return <li>
        {name}
    </li>;
}

function DecksList () {
    const decks = useContext(DecksDataContext);

    return <ul>
        {
            decks.map(el=><li key={el._id}>
                <Link to={`/decks/${el._id}`}>{el.name}</Link>
            </li>)
        }
    </ul>;
}