import React, { useContext, useEffect, useState } from "react";
import Page from './Page';
import UserContext from './context/UserContext';
import { Link, Switch, Route } from 'react-router-dom';
import FirebaseContext from './context/FirebaseContext';
import { ProfileDataContext } from './context/UserDataContexts';

import imageCompression from 'browser-image-compression';

import profile from '../static/placeholder.png';

import './css/profile';

export default function CurrentPage() {
    const user = useContext(UserContext);
    const firebase = useContext(FirebaseContext);
    // u - private user data; p - public user data
    const pData = useContext(ProfileDataContext);
    const [uploading, setUploading] = useState(false);

    function handleDrop(e) {
        e.stopPropagation();
        e.preventDefault();

        if (e.dataTransfer.files) if (e.dataTransfer.files.length > 0) {
            const file = e.dataTransfer.files[0];
            if (file.type.match(/image\//g)) {
                setUploading('Compressing ...');
                imageCompression(file, {
                    maxSizeMB: 1,
                    maxWidthOrHeight: 200,
                }).then((cfile)=>{
                    var storageRef = firebase.storage().ref();
                    var profileRef = storageRef.child(`profiles/${user.uid}.${file.name.split('.').pop().toLowerCase()}`);
                    setUploading('Uploading ...');
                    
                    var upload = profileRef.put(cfile);

                    upload.then(snapshot => {
                        snapshot.ref.getDownloadURL().then(async function (downloadURL) {
                            console.log('updating profile');
                            setUploading('Applying ...');

                            await firebase.firestore().collection("profiles").doc(user.uid).update({
                                profilePic: downloadURL
                            });

                            setUploading(false);
                        });
                    });

                    upload.on('state_changed', function (snapshot) {

                        var progress = Math.floor((snapshot.bytesTransferred / snapshot.totalBytes) * 100);
                        console.log('Progress: ' + progress + '%');
                        setUploading(`Uploading (${progress} %)...`);
                    });
                })
                .catch(e=>{
                    setUploading('Compression failed.')
                })
            }
        }
    }

    function stopDefault(e) {
        e.stopPropagation();
        e.preventDefault();
    }

    return (
        <Page>
            {
                pData ?
                    <div className="profile">
                        <div className="header">
                            <div className="profile-image"
                                onDrop={handleDrop}
                                onDragOver={stopDefault}
                                onDragLeave={stopDefault}
                            >
                                <img src={pData.profilePic || profile} />
                                {
                                    uploading ? <div className="image-upload" style={{ backgroundColor: 'rgba(0,0,0,50)', opacity: '1' }}>
                                        <span style={{ color: 'white', fontFamily: '\'Montserrat\', sans-serif' }}>{uploading}</span>
                                    </div> :
                                        <div className="image-upload" />
                                }
                            </div>
                            <div className="wrapper">
                                <div className="display-name">{pData.displayName || 'Anonymous Trainer'}</div>
                                <div className="bio">{pData.bio || ''}</div>
                                <div className="stats">
                                    <div className="friends">{pData.friends ? `${pData.friends.length} friends` : undefined}</div>
                                </div>
                            </div>
                        </div>
                        <div className="quick-actions">
                            <ul>
                                <li>
                                    <Link to="/profile/update">Update Profile</Link>
                                </li>
                                <li>
                                    <Link to="/profile/friends">Friends</Link>
                                </li>
                                <li onClick={() => firebase.auth().signOut()}>
                                    Sign Out
                                </li>
                            </ul>
                        </div>
                        <Switch>
                            <Route exact path="/profile/update" component={UpdateProfile} />
                            <Route exact path="/profile/friends" component={FriendsList} />
                            <Route exact path="/profile/friends/add" component={FriendAdd} />
                            <Route exact path="/profile/link" component={LinkAccount} />
                        </Switch>
                    </div> : <h2>Loading ...</h2>
            }
        </Page>
    );
}

function LinkAccount () {
    const firebase = useContext(FirebaseContext);
    const [ guid, setGuid ] = useState(__inject.client_id);
    const [ error, setError ] = useState(false);

    function handleLink () {
        var linkAccount = firebase.functions().httpsCallable('linkAccount');
        linkAccount({client_id: guid}).then(console.log).catch(e=>{
            setError(e.message);
        });
    }

    return <>
        <div className="link-account">
            <input type="text" value={guid} onChange={e=>setGuid(e.target.value)} />
            {
                error ? <span>{error}</span> : undefined
            }
            <button onClick={handleLink}>Link Account</button>
        </div>
    </>
}

function FriendAdd () {
    const firebase = useContext(FirebaseContext);
    const [ query, setQuery ] = useState('');
    const [ candidates, setCandidates ] = useState([]);

    function handleSearchInput (e) {
        setQuery(e.target.value);
    }

    function handleSearch () {
        firebase.firestore().collection('profiles').where('displayName', '==', query).limit(20).get().then(snap=>{
            var newState = [];
            snap.forEach(doc=>{
                newState.push({...doc.data(), _id: doc.id});
                
            });
            setCandidates(newState);
        })
    }

    return <>
        <h2>Add a Friend</h2>
        <div className="search-bar">
            <input type="text" onChange={handleSearchInput} value={query} />
            <button onClick={handleSearch}>Search</button>
        </div>
        <ul>
            {
                candidates.map(dat=><SearchResult data={dat} key={dat._id} />)
            }
        </ul>
        
    </>
}

function SearchResult({ data }) {
    const firebase = useContext(FirebaseContext);
    const [ state, setState ] = useState('');

    function handleSendRequest () {
        var sendInvite = firebase.functions().httpsCallable('sendInvite');
        setState('Sending ...');
        sendInvite({invite_id: data._id}).then(()=>setState('Sent the invite!')).catch(e=>{
            setState('Could not send the invite. Reason: ' + e.message)
        })
    }

    return <li>
        {
            state ? 
                <span>{state}</span> : 
                <>
                    <img src={data.profilePic || profile} />
                    <span>{data.displayName}</span>
                    <button onClick={handleSendRequest}>Send Request</button>
                </>
        }
    </li>
}



function FriendsList () {
    const pData = useContext(ProfileDataContext);
    const firebase = useContext(FirebaseContext);
    const [ friends, setFriends ] = useState([]);

    useEffect(()=>{
        if (!pData) return console.error('The account is invalid!');

        if (!Array.isArray(pData.friends)) return;

        let newState = [];

        pData.friends.forEach(el=>{
            firebase.firestore().collection('profiles').doc(el).get().then((doc)=>{
                newState.push({...doc.data(), _id: doc.id});
                setFriends([...newState]);
            });
        });
    }, [pData]);

    return (
        <>
            <div className="add-friend"><Link to="/profile/friends/add">Add friends</Link></div>
            <ul>
                {
                    friends.map(dat=><li key={dat._id}>
                        <img src={dat.profilePic || profile} />{dat.displayName}
                    </li>)
                }
            </ul>
        </>
    )
}

function UpdateProfile () {
    const user = useContext(UserContext);
    const firebase = useContext(FirebaseContext);
    
    const [ displayName, setDisplayName ] = useState('');
    const [ bio, setBio ] = useState('');

    // Get the data 
    useEffect(() => {
        firebase.firestore().collection('profiles').doc(user.uid).get().then(doc => {
            if (doc.data().bio && doc.data().displayName) {
                setDisplayName(doc.data().displayName);
                setBio(doc.data().bio);
            }
        });
    }, [firebase, user]);

    function handleSubmit(e) {
        e.stopPropagation();
        e.preventDefault();

        firebase.firestore().collection('profiles').doc(user.uid).update({
            displayName: displayName,
            bio: bio
        });
    }

    return <>
        <form onSubmit={handleSubmit}>
            <input type="text" onChange={e=>setDisplayName(e.target.value)} value={displayName} />
            <textarea onChange={e=>setBio(e.target.value)} value={bio} />
            <input type="submit" value="Update" />
        </form>
    </>
}