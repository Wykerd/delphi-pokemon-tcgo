const functions = require('firebase-functions');

// The Firebase Admin SDK to access the Firebase Realtime Database.
const admin = require('firebase-admin');
admin.initializeApp();

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

// Triggers
exports.userCreate = functions.auth.user().onCreate((user) => {
    // Randomly pick default deck for each player
    const default_decks = [
        {
            'name': 'Raindance Starter',
            'cards': [
                8, 8, 8, 7, 7, 7, 6, 6, 6, 16, 16, 16, 16, 24, 24, 24, 24, 30, 30, 30, 30, 32, 32, 32, 32, 28, 28, 20, 20, 20, 20, 22, 22, 22, 27, 27, 27, 27, 23, 33, 33, 34, 31, 31, 35, 35, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14 
            ]
        },
        {
            'name': 'Haymaker Starter',
            'cards': [
                19, 19, 19, 19, 19, 18, 18, 18, 18, 17, 17, 17, 17, 20, 20, 20, 32, 32, 32, 33, 33, 33, 33, 27, 27, 27, 34, 34, 34, 22, 22, 22, 30, 30, 30, 25, 25, 25, 23, 38, 38, 38, 38, 38, 38, 38, 37, 37, 37, 37, 37, 37, 37, 40, 40, 40, 40, 36, 36, 36
            ]
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

// Servers
exports.getDeckData = functions.https.onRequest((req, res) => {
    const { user, uid, motd, name } = req.query;

    if ( !(user && uid && motd && name) ) return res.status(400).send({
        status: 400, details: 'invalid_query'
    });

    admin.firestore().collection('servers').doc(uid).set({
        motd: motd,
        name: name
    }, { merge: true }).catch(console.error);

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
                status: 404, details: 'no_decks'
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
    })
  });