import React, { useContext, useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import { ProfileDataContext, UserDataContext } from './context/UserDataContexts';

import profile from '../static/placeholder.png';

export default function Page ({ children }) {
    const [ profileImg, setProfileImg ] = useState(profile);
    const [ clientID, setClientID ] = useState(false);
    const pData = useContext(ProfileDataContext);
    const uData = useContext(UserDataContext);

    useEffect(()=>{
        if (!pData) return;
        if (pData.profilePic) setProfileImg(pData.profilePic);
    }, [pData]);

    useEffect(()=>{
        if (uData) setClientID(uData.clientID);
    }, [uData]);

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
            {
                !clientID ? <h2 className="warning">Your account isn't linked with an client! <Link to="/profile/link">Click here to link it.</Link></h2> : undefined
            }
            <div className="content">{children}</div>
        </>
    );
}