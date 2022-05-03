import "./style.css";
import { Elm } from "./src/Main.elm";
import './src/index.css';

const root = document.querySelector("#app div");
Elm.Main.init({ node: root });