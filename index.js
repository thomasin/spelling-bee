import styles from './src/main.css'
import { Elm } from './src/Main.elm'

document.addEventListener("DOMContentLoaded", contentLoaded)

function contentLoaded () {
    var app = Elm.Main.init()
}