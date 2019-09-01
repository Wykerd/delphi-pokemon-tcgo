import React from "react";

const UserContext = React.createContext({
    loggedIn: false,
    email: '',
    uid: ''
});

export default UserContext;