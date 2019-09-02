import React, { useEffect, useState, useContext } from "react";
import { BrowserRouter as Router, Route, Switch } from "react-router-dom";
import Landing from './Landing';
import Login from './Login';
import Home from './Home';
import CurrentProfile from './CurrentProfile';
import UserContext from './context/UserContext';
import FirebaseContext from './context/FirebaseContext';

export default () => {
  const firebase = useContext(FirebaseContext);
  const [user, setUser] = useState(null);

  useEffect(() => {
    firebase.auth().onAuthStateChanged(setUser);
  }, []);

  return (
    <UserContext.Provider value={user}>
      <Router>
        
        <Switch>
          <Route exact path="/" component={Landing} />
          {
            user ? 
            <>
            <Route exact path="/login" component={Login} />
            <Route exact path="/home" component={Home} />
            <Route path="/profile" component={CurrentProfile} /> 
            </>:
            <Route path="/" component={Login} />
          }
        </Switch>
      </Router>
    </UserContext.Provider>
  );
}