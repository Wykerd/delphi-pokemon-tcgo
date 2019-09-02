import React, { useContext, useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import UserContext from './context/UserContext';
import FirebaseContext from './context/FirebaseContext';

import profile from '../static/placeholder.png';

export default function Page ({ children }) {
    const user = useContext(UserContext);
    const firebase = useContext(FirebaseContext);
    const [ profileImg, setProfileImg ] = useState(profile);

    useEffect(()=>{
        firebase.firestore().collection('profiles').doc(user.uid).onSnapshot(doc=>{
            if (doc.data()) if (doc.data().profilePic) setProfileImg(doc.data().profilePic);
        });
    }, []);

    return (
        <>
            <header>
                <ul>
                    <li>
                       <Link to="/home">Home</Link>
                    </li>
                    <li>
                       <Link to="/decks">Decks</Link> 
                    </li>
                    <li>
                       <Link to="/trade">Trade</Link>
                    </li>
                    <li>
                        <Link to="/profile">
                            <img src={profileImg} />
                        </Link>
                    </li>
                </ul>
            </header>
            <div className="content">{children}</div>
        </>
    );
}