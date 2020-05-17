import styles from './src/main.css'
import { Elm } from './src/Main.elm'

document.addEventListener("DOMContentLoaded", contentLoaded)

function contentLoaded () {
    var app = Elm.Main.init()

    function toPot (keyLetter, otherLetters) {
        var sortedLetters = [...otherLetters].sort()
        return keyLetter + sortedLetters.join("")
    }

    app.ports.checkInStorage.subscribe(data => {
        var pot = toPot(data.keyLetter, data.otherLetters)
        var guesses = localStorage.getItem(pot) || "[]"
        app.ports.foundInStorage.send({
            ...data,
            guesses: JSON.parse(guesses),
        })
    })

    app.ports.putInStorage.subscribe(data => {
        var pot = toPot(data.keyLetter, data.otherLetters)
        localStorage.setItem(pot, JSON.stringify(data.guesses))
    })

    app.ports.instantScrollToBottom.subscribe(scrollerId => {
        requestAnimationFrame(() => {
            console.log("scrolling instant " + scrollerId)
            var wordListDropdown = document.querySelector(scrollerId)
            wordListDropdown.scroll(0, wordListDropdown.scrollHeight)
        })
    })
}
