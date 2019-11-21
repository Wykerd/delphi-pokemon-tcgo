import React, { useState, useEffect } from "react";
import { Redirect } from 'react-router-dom';

import './css/landing';

export default function Landing () {
    const [ landing, setLanding ] = useState(true);

    useEffect(()=>{
        // Gives me enough time to enject javascript into the chromium window
        setTimeout(()=>setLanding(false), 5000);
    }, []);

    return (
        <>
        {
            landing ? <div className="landing-page">
                This is a fanmade website and is not associated with Nintendo or Gamefreak
            </div> : <Redirect to="/login" />
        }
        </>
    )
}