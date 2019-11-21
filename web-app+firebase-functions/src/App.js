import React, { useEffect, useState, useContext } from "react";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import Landing from './Landing';
import Login from './Login';
import Home from './Home';
import Trade from './Trade';
import Decks from './Decks';
import CurrentProfile from './CurrentProfile';
import UserContext from './context/UserContext';
import FirebaseContext from './context/FirebaseContext';
import { UserDataContext, ProfileDataContext, LogDataContext, DecksDataContext } from './context/UserDataContexts';

export default () => {
  const firebase = useContext(FirebaseContext);
  const [user, setUser] = useState(null);

  useEffect(() => {
    firebase.auth().onAuthStateChanged(setUser);
  }, [firebase]);

  const [ uData, setUData ] = useState(undefined);
  const [ pData, setPData ] = useState(undefined);
  const [ logData, setLogData ] = useState([]);
  const [ decksData, setDecksData ] = useState([]);

  useEffect(()=>{
      if (!user) return;

      firebase.firestore().collection("users").doc(user.uid).onSnapshot(function(doc) {
          setUData({...doc.data(), _id: doc.id});
      });

      firebase.firestore().collection('profiles').doc(user.uid).onSnapshot(doc=>{
          setPData({...doc.data(), _id: doc.id});
      });

      firebase.firestore().collection('profiles').doc(user.uid).collection('decks').onSnapshot(snap=>{
        let newState = [];
          snap.forEach(doc=>{
              newState.push({...doc.data(), _id: doc.id});
          });
          setDecksData(newState);
      })

      firebase.firestore().collection('logs').orderBy("timestamp", "desc").where('user', '==', firebase.firestore().collection("users").doc(user.uid)).limit(10).onSnapshot(snap=>{
          let newState = [];
          snap.forEach(doc=>{
              newState.push({...doc.data(), _id: doc.id});
              
          });
          setLogData(newState);
      });
  }, [firebase, user]);    

  return (
    <UserContext.Provider value={user}>
      <UserDataContext.Provider value={uData}>
        <ProfileDataContext.Provider  value={pData}>
          <LogDataContext.Provider value={logData}>
            <DecksDataContext.Provider value={decksData}>
              <Router>
                <Switch>
                  <Route exact path="/" component={Landing} />
                  {
                    user ? 
                    <>
                    <Route exact path="/login" component={Login} />
                    <Route exact path="/home" component={Home} />
                    <Route path="/profile" component={CurrentProfile} /> 
                    <Route path="/trade" component={Trade} />
                    <Route path="/decks" component={Decks} />
                    </>:
                    <Route path="/" component={Login} />
                  }
                </Switch>
              </Router>
            </DecksDataContext.Provider>
          </LogDataContext.Provider>
        </ProfileDataContext.Provider>
      </UserDataContext.Provider>
    </UserContext.Provider>
  );
}