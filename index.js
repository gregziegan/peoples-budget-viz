import "./style.css";
// @ts-ignore
const { Elm } = require("./src/Main.elm");
const pagesInit = require("elm-pages");

pagesInit({
  mainElmModule: Elm.Main
}).then(app => {
  var dimensions = { 'width': window.innerWidth, 'height': window.innerHeight };
  app.ports.windowSize.send(dimensions);
});
