const functions = require('firebase-functions');

// The Firebase Admin SDK to access the Firebase Realtime Database.
const admin = require('firebase-admin');
admin.initializeApp();

const API_VERSION = '1';
/*
THE HTTP API
*/

// Servers

// && motd && name
/*
admin.firestore().collection('servers').doc(uid).set({
        motd: motd,
        name: name
    }, { merge: true }).catch(console.error);
*/
exports.registerServer = functions.https.onRequest((req, res) => {
    const { uid, motd, name, custom_name, git_repo, website, version_name, 
        spec_api_version, spec_server_version, spec_client_version,
        spec_legacy_support, spec_backwards_compatibility } = req.query;
    
    admin.firestore().collection('servers').doc(uid).set({
        motd: motd,
        name: name,
        custom_name: custom_name,
        git_repo: git_repo,
        website: website,
        version_name: version_name,
        spec_api_version: spec_api_version,
        spec_server_version: spec_server_version,
        spec_client_version: spec_client_version,
        spec_legacy_support: spec_legacy_support === 'true',
        spec_backwards_compatibility: spec_backwards_compatibility
    }, { merge: true })
    .catch(console.error);

    if (spec_api_version !== API_VERSION) {
        res.status(200).send({
            status: 200,
            message: 'registration_ok',
            warning: `The server is running on an diffirent API spec version which could cause errors. Please use a server that supports the API spec v.${API_VERSION}`
        });   
    } else {
        res.status(200).send({
            status: 200,
            message: 'registration_ok'
        });    
    }
    
});

// getDeckData
// Required query parameters
// user: guid of user
// uid: guid of server
exports.getDeckData = functions.https.onRequest((req, res) => {
    const { user, uid } = req.query;

    if ( !(user && uid) ) return res.status(400).send({
        status: 400, message: 'invalid_query'
    });

    admin.firestore().collection('servers').doc(uid).get().then(_d=>{
        admin.firestore().collection('users').where('clientID', '==', user).limit(1).get().then(snap=>{
            if (snap.docs.length < 1) return res.status(404).send({
                status:404, message:'user_undefined'
            });
    
            const user_id = snap.docs[0].id;
    
            admin.firestore().collection('profiles').doc(snap.docs[0].id).collection('decks').get().then(snap=>{
                const deckData = [];
    
                snap.forEach(doc=>{
                    deckData.push({ ...doc.data(), _id: doc.id });
                });
    
                if (deckData.length < 1) return res.status(404).send({
                    status: 404, message: 'no_decks'
                })
    
                admin.firestore().collection('logs').add({
                    timestamp: new Date().getTime(),
                    action: 'Get deck data',
                    server:  admin.firestore().collection('servers').doc(uid),
                    user: admin.firestore().collection('users').doc(user_id)
                });
    
                res.status(200).send({
                    status: 200,
                    data: deckData
                });
            })
        }).catch(_e=>res.status(404).send({
            status:404, message:'user_undefined'
        }));    
    }).catch(_e=>res.status(404).send({
        status: 404, message: 'server_not_registered'
    }))
});

/*
THE WEB APP FUNCTIONS
*/

exports.declineInvite = functions.https.onCall((data, context)=>{
    if (!context.auth) {
        throw new functions.https.HttpsError('failed-precondition', 'The function must be called while authenticated.');
    }

    const uid = context.auth.uid;

    if (!data) throw new functions.https.HttpsError('invalid-argument', 'No data');

    const { invite_id } = data;

    if (!invite_id) throw new functions.https.HttpsError('invalid-argument', 'Required data invite_id not found');

    const prom1 = admin.firestore().collection('users').doc(invite_id).update({
        sent_requests: admin.firestore.FieldValue.arrayRemove(uid)
    });
    
    const prom2 = admin.firestore().collection('users').doc(uid).update({
        friend_requests: admin.firestore.FieldValue.arrayRemove(admin.firestore().collection('profiles').doc(invite_id)),
    });

    return Promise.all([prom1, prom2]);
});

exports.acceptInvite = functions.https.onCall((data, context) => {
    if (!context.auth) {
        throw new functions.https.HttpsError('failed-precondition', 'The function must be called while authenticated.');
    }

    const uid = context.auth.uid;

    if (!data) throw new functions.https.HttpsError('invalid-argument', 'No data');

    const { invite_id } = data;

    if (!invite_id) throw new functions.https.HttpsError('invalid-argument', 'Required data invite_id not found');

    // Check if the other user sent an invite
    return admin.firestore().collection('users').doc(invite_id).get().then(snap=>{
        const data = snap.data();
        
        if (!data) {
            throw new functions.https.HttpsError ('invalid-argument', 'The user does not exist')
        } 

        if (!Array.isArray(data.sent_requests)) {
            throw new functions.https.HttpsError ('internal', 'The user data is corrupted')
        }

        if (!data.sent_requests.includes(uid)) {
            throw new functions.https.HttpsError ('permission-denied', 'The user did not send an request')
        }

        const prom1 = admin.firestore().collection('users').doc(invite_id).update({
            sent_requests: admin.firestore.FieldValue.arrayRemove(uid)
        });

        const prom2 = admin.firestore().collection('profiles').doc(invite_id).update({
            friends: admin.firestore.FieldValue.arrayUnion(uid)
        });

        const prom3 = admin.firestore().collection('users').doc(uid).get().then(s=>{
            const d = s.data();

            if (Array.isArray(d.friend_requests)) if (d.friend_requests.includes(invite_id)) {
                d.friend_requests.splice(d.friend_requests.indexOf(invite_id), 1);
            }

            const prom1 = admin.firestore().collection('users').doc(uid).update({
                friend_requests: admin.firestore.FieldValue.arrayRemove(admin.firestore().collection('profiles').doc(invite_id)),
            });

            const prom2 = admin.firestore().collection('profiles').doc(uid).update({
                friends: admin.firestore.FieldValue.arrayUnion(invite_id)
            });

            return Promise.all([prom1, prom2])
        })

        return Promise.all([prom1, prom2, prom3])
    })
});

exports.sendInvite = functions.https.onCall((data, context) => {
    if (!context.auth) {
        throw new functions.https.HttpsError('failed-precondition', 'The function must be called while authenticated.');
    }

    const uid = context.auth.uid;

    if (!data) throw new functions.https.HttpsError('invalid-argument', 'No data');

    const { invite_id } = data;

    if (!invite_id) throw new functions.https.HttpsError('invalid-argument', 'Required data invite_id not found');

    return admin.firestore().collection('users').doc(invite_id).get().then(snap=>{
        const data = snap.data();
        
        if (!data) {
            throw new functions.https.HttpsError ('invalid-argument', 'The user does not exist')
        } 

        const prom1 = admin.firestore().collection('users').doc(invite_id).update({
            friend_requests: admin.firestore.FieldValue.arrayUnion(admin.firestore().collection('profiles').doc(uid))
        });

        const prom2 = admin.firestore().collection('users').doc(uid).update({
            sent_requests: admin.firestore.FieldValue.arrayUnion(invite_id)
        })

        return Promise.all([prom1, prom2]);
    });
});

exports.linkAccount = functions.https.onCall((data, context) => {
    if (!context.auth) {
        throw new functions.https.HttpsError('failed-precondition', 'The function must be called while authenticated.');
    }

    const uid = context.auth.uid;

    if (!data) throw new functions.https.HttpsError('invalid-argument', 'No data');

    const { client_id } = data;

    if (!client_id) throw new functions.https.HttpsError('invalid-argument', 'Required data client_id not found');

    return admin.firestore().collection('users').where('clientID', '==', client_id).limit(1).get().then(snap=>{
        if (snap.docs.length > 0) throw new functions.https.HttpsError('permission-denied', 'Another account is already linked with that client id');

        return admin.firestore().collection('users').doc(uid).get().then(snap=>{
            const data = snap.data();
            
            if (!data) {
                throw new functions.https.HttpsError ('not-found', 'The user does not exist')
            } 

            return admin.firestore().collection('users').doc(uid).update({
                clientID: client_id
            });
        });
    })
});

// Triggers
exports.userCreate = functions.auth.user().onCreate((user) => {
    // Randomly pick default deck for each player
    const default_decks = [
        {
            'name': 'Raindance Starter',
            'cards': [8,8,8,8,8,8,7,7,7,7,7,7,6,6,6,6,6,6,16,16,16,16,16,16,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42]
        },
        {
            'name': 'Haymaker Starter',
            'cards': [19,19,19,19,19,19,18,18,18,18,18,17,17,17,17,17,38,38,38,38,38,38,38,38,37,37,37,37,37,37,37,37,40,40,40,40,40,36,36,36]
        }
    ];

    const deck = Math.round(Math.random() * (default_decks.length - 1));

    const prom1 = admin.firestore().collection('profiles').doc(user.uid).set({
        friends: [],
        bio: 'Gotta catch em all',
        profilePic: user.photoURL || '',
        displayName: user.displayName || 'Anonymous Trainer',
    }).then(()=>{
        admin.firestore().collection('profiles').doc(user.uid).collection('decks').doc('default').set(default_decks[deck])
        .catch(console.error);
    }).catch(console.error);

    const prom2 = admin.firestore().collection("users").doc(user.uid).set({
        clientID: '',
        friend_requests: [],
        sent_requests: [],
        cards: default_decks[deck].cards
    }).catch(console.error);

    return Promise.all([prom1, prom2]);
});