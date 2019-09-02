const functions = require('firebase-functions');

// The Firebase Admin SDK to access the Firebase Realtime Database.
const admin = require('firebase-admin');
admin.initializeApp();


// // Create and Deploy Your First Cloud Functions
// // https://firebase.google.com/docs/functions/write-firebase-functions
//
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