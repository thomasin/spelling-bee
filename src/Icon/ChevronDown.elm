module Icon.ChevronDown exposing (render)

import Svg
import Svg.Attributes as Attr


render =
    Svg.svg
        [ Attr.fill "none"
        , Attr.strokeLinecap "round"
        , Attr.strokeLinejoin "round"
        , Attr.strokeWidth "2"
        , Attr.stroke "currentColor"
        , Attr.viewBox "0 0 24 24"
        , Attr.width "24"
        , Attr.height "24"
        ]
        [ Svg.path [ Attr.d "M19 9l-7 7-7-7" ] []
        ]