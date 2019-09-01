import * as firebase from "firebase/app";

// Firebase services used
import "firebase/firestore";
import "firebase/auth";
import "firebase/storage";

// Config from https://console.firebase.google.com/u/0/project/entersekt-koffie/settings/general/
const firebaseConfig = {
    apiKey: "AIzaSyD3ugbZncs-O1QQxP__8plciP2jMfOh3pE",
    authDomain: "entersekt-koffie.firebaseapp.com",
    databaseURL: "https://entersekt-koffie.firebaseio.com",
    projectId: "entersekt-koffie",
    storageBucket: "entersekt-koffie.appspot.com",
    messagingSenderId: "437000617922",
    appId: "1:437000617922:web:7fe6965aee223d47"
  };

firebase.initializeApp(firebaseConfig);

const Firebase = firebase;

export default Firebase;