module Icon.Refresh exposing (render)

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
        [ Svg.path [ Attr.d "M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" ] []
        ]