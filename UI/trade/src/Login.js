import React, { useContext, useState } from "react";
import { Redirect } from 'react-router-dom';
import UserContext from "./context/UserContext";
import FirebaseContext from './context/FirebaseContext';

export default function Login () {
    const user = useContext(UserContext);
    const firebase = useContext(FirebaseContext);

    const [ data, setData ] = useState({
        'email': '',
        'password': ''
    });

    function handleSubmit(e) {
        e.stopPropagation();
        e.preventDefault();
    }

    function handleChange (e) {
        let newState = {};
        Object.assign(newState, data);

        newState[e.target.name] = e.target.value;

        setData(newState);
    }

    function handleSignIn () {
        firebase.auth().signInWithEmailAndPassword(data.email, data.password).catch(e=>{
            console.warn(e.message);
        });
    }

    function handleSignUp () {
        firebase.auth().createUserWithEmailAndPassword(data.email, data.password).catch(e=>{
            console.warn(e.message);
        });
    }

    return (
        <>
        {
            user ? <Redirect to="/home" /> : <div className="login-page">
                <div className="login-wrap">
                    <form onSubmit={handleSubmit}>
                        <input onChange={handleChange} name="email" value={data.email} type="text" />
                        <input onChange={handleChange} name="password" value={data.password} type="password" />
                        <button onClick={handleSignIn} id="login-button">Sign In</button>
                        <button onClick={handleSignUp} id="sign-up-button">Don't have an account? Sign Up.</button>
                    </form>
                </div>
            </div>
        }
        </>
    )
}