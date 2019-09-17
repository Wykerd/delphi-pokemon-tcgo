import * as firebase from "firebase/app";

// Firebase services used
import "firebase/firestore";
import "firebase/auth";
import "firebase/storage";
import "firebase/functions";

// Config from https://console.firebase.google.com/u/0/project/entersekt-koffie/settings/general/
const firebaseConfig = {
  apiKey: "AIzaSyDGOEHQS2uyOA63wu3fy3ymrnKfc6sMZ8A",
  authDomain: "pokemon-central.firebaseapp.com",
  databaseURL: "https://pokemon-central.firebaseio.com",
  projectId: "pokemon-central",
  storageBucket: "pokemon-central.appspot.com",
  messagingSenderId: "927138649338",
  appId: "1:927138649338:web:e4f2d61c4ba95fc1"
};

firebase.initializeApp(firebaseConfig);

const Firebase = firebase;

export default Firebase;