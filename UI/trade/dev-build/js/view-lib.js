// Copyright 2019, Daniel Wykerd
// Very lightweight view library

// Make Element    attribute                inner elements  Namespace
function mkEl(tagname, attrs, events, styles, innerEls, NS) {
    var el;

    // for smaller uglifyjs results
    var 
        typeInvalid = 'Invalid type',
        objectType = 'object',
        stringType = 'string'


    // If namespace defined use it
    NS ? el = document.createElementNS(NS, tagname) : el = document.createElement(tagname);

    // Set attributes
    if (typeof attrs === objectType)
        for (let attr in attrs) {
            el.setAttribute(attr, attrs[attr]);
        }
    else console.warn('attrs>' + typeInvalid);

    // Add events
    if (typeof events === objectType)
        for (let event in events) {
            if (typeof events[event] === 'function') {
                el.addEventListener (event, events[event]);
            } else console.warn('events>' + typeInvalid);
        }

    // Add styles
    if (typeof styles === objectType) 
        for (var style in styles) {
            if (style in el.style) 
                el.style[style] = styles[style]
            else console.warn(style + ' is invalid');
        }

    // Append inner elements
    if (Array.isArray(innerEls)) {
        for (let i = 0; i < innerEls.length; i++) {
            if (innerEls[i] instanceof window.Element) {
                el.appendChild(innerEls[i]);
            } else if (typeof innerEls[i] === stringType) {
                var textNode = document.createTextNode(innerEls[i]);
                el.appendChild(textNode);
            } else console.warn ('el>' + typeInvalid);
        }
    } else if (typeof innerEls === objectType || typeof innerEls === stringType ) {
        if (innerEls instanceof window.Element) {
            el.appendChild(innerEls);
        } else if (typeof innerEls === 'string') {
            var textNode = document.createTextNode(innerEls);
            el.appendChild(textNode);
        } else console.warn ('el>' + typeInvalid);
    } 

    return el;
}

function div(attrs, events, styles, innerEls, NS) {
    return mkEl('div', attrs, events, styles, innerEls, NS)
}

function svgUse(id, className, events, styles) {
    //Better uglify results
    var svgns = 'http://www.w3.org/2000/svg';

    var useElem = document.createElementNS(svgns, 'use');
    useElem.setAttributeNS('http://www.w3.org/1999/xlink', 'xlink:href', id);

    var el = mkEl('svg', {class:className}, events, styles, useElem, svgns);

    return el;
}