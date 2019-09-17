import React from "react";
import ReactDOM from "react-dom";
import App from "./App";

import 'normalize.css';
import './css/index.css';

// Firebase
import FirebaseContext from './context/FirebaseContext';
import Firebase from './Firebase';

ReactDOM.render(
    <FirebaseContext.Provider value={Firebase}>
        <App />
    </FirebaseContext.Provider>, 
        
    document.getElementById('root'));
