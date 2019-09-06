import React, { useContext, useEffect, useState } from "react";
import Page from './Page';
import FirebaseContext from './context/FirebaseContext';
import './css/home';
import { UserDataContext, LogDataContext } from './context/UserDataContexts';

import profile from '../static/placeholder.png';

export default function Home () {
    // u - private user data; p - public user data
    const uData = useContext(UserDataContext);
    const logData = useContext(LogDataContext);

    return (
        <Page>
            <h1>Home</h1>
            {
                uData ? 
                <>
                    {
                        uData.friend_requests.length > 0 ? <h2>Friend Requests</h2> : undefined
                    }
                    {
                        uData.friend_requests.map((d,i)=>
                            <FriendRequest data={d} key={i} />
                        )
                    }
                    {
                        logData.length > 0 ? <h2>Servers Access Logs</h2> : undefined
                    }
                    {
                        logData.map(dat=><LogItem data={dat} key={dat._id} />)
                    }
                </>:
                <span className="loading">Loading ...</span>
            }
        </Page>
    )
}

function LogItem ({ data }) {
    const [ serverDat, setServerDat ] = useState({name: 'Loading ...', action: 'Loading ...', _id: ''});

    useEffect(()=>{
        data.server.get().then(dat=>setServerDat({...dat.data(), _id: dat.id}));
    }, []);

    return (
        <div className="log-item">
            <div className="server-name">Server: {serverDat.name}</div>
            <div className="action">Action: {data.action}</div>
        </div>
    )
}

function FriendRequest ({ data }) {
    const firebase = useContext(FirebaseContext);
    const [ dat, setDat ] = useState(false);
    const [ error, setError ] = useState(undefined);
    const [ loading, setLoading ] = useState(false);

    useEffect(()=>{
        data.get().then(snap=>setDat(snap.data()));
    }, []);

    function handleAccept () {
        var acceptInvite = firebase.functions().httpsCallable('acceptInvite');
        setLoading(true);
        acceptInvite({invite_id: data.id}).then(console.log).catch(e=>{
            setError(e.message);
            setLoading(false);
        })
    }

    function handleDecline () {
        var declineInvite = firebase.functions().httpsCallable('declineInvite');
        declineInvite({invite_id: data.id}).then(console.log).catch(e=>{
            console.log(e.message)
        })
    }
    
    if (loading) return (
        <div className="friend-request message">
            Processing ...
        </div>
    )

    if (error) return (
        <div className="friend-request message">
            {error}
        </div>
    )

    if (dat) if (dat.displayName && dat.bio && (dat.profilePic !== undefined)) return (
        <div className="friend-request">
            <div className="image" style={{backgroundImage: `url('${dat.profilePic || profile}')`}} />
            <div className="info">
                <span className="display-name">{dat.displayName}</span>
                <p>{dat.bio}</p>
            </div>
            <button className="accept" onClick={handleAccept}>Accept</button>
            <button className="decline" onClick={handleDecline}>Decline</button>
        </div>
    );

    return <></>;
}