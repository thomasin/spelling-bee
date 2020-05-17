module Component.Hexagon exposing (render)

styles =
    [ "font-sans"
    , "font"
    ]

render letter isKey =
    Html.div
        [ Attr.classList [] ]

hexagon letter isKey =
            Html.div [ Attr.classList [("hexagon font-sans", True), ("hex-key", isKey)] ]
                [ Html.text <| String.fromChar letter ]