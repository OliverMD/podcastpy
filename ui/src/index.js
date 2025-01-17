'use strict';

require("./styles.css");

const {Elm} = require('./Main');
var app = Elm.Main.init({flags: 6, node: document.getElementById("main")});

app.ports.toJs.subscribe(data => {
    console.log(data);
})
// Use ES2015 syntax and let Babel compile it for you
var testFn = (inp) => {
    let a = inp + 1;
    return a;
}