port module Main exposing (..)

import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Url
import Url.Parser as Parser
import Html
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Random
import Array
import Set
import Task
import Process
import Json.Decode as Decoder
import Json.Encode as Encoder
import Random.List

import Toasty
import Toasty.Defaults
import Color
import Scale.Color as Scale

import Icon.ChevronDown
import Icon.Refresh


--


port foundInStorage : (Decoder.Value -> msg) -> Sub msg
port checkInStorage : Encoder.Value -> Cmd msg
port putInStorage : Encoder.Value -> Cmd msg
port instantScrollToBottom : String -> Cmd msg


toastyConfig =
    Toasty.Defaults.config
        |> Toasty.delay 5000


type alias Model =
    { key : Nav.Key
    , state : AppState
    }

    
type AppState
    = FindingPots
    | ChoosingPot
    | ChoosingKeyLetter
    | FindingWords
    | CheckingStorage
    | Loaded Honeycomb
    | Problem String


type alias Data =
    { pots : List String
    , words : List String
    }


type alias Honeycomb =
    { input : List Char
    , keyLetter : Char
    , otherLetters : List Char
    , possibleWords : List String
    , guesses : List String
    , wordListExpanded : Bool
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | FoundPots (Result Http.Error String)
    | FoundWords (Char, List Char) (Result Http.Error String)
    | ChosenPot (Maybe String)
    | ChosenKeyLetter (Maybe Char, List Char)
    | FoundInStorage (Result Decoder.Error StorageCheckResult)
    | LetterClicked Char
    | LetterRemoved
    | GuessMade
    | ShuffleHoneycomb
    | ShuffleLetters
    | LettersShuffled (List Char)
    | WordListDropdownToggled
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
      route key (potFromUrl url)


oneToN : Int -> Random.Generator Int
oneToN n =
  Random.int 1 n


choosePot pots =
    Random.List.choose pots
        |> Random.map Tuple.first
        |> Random.generate ChosenPot


chooseKeyLetter pot =
    String.toList pot
        |> Random.List.choose
        |> Random.generate ChosenKeyLetter


checkLetters chars word =
    case String.uncons word of
        Just (letter, restOfWord) ->
            if List.member letter chars then
                checkLetters chars restOfWord

            else
                False

        Nothing ->
            True


wordIsPossible (keyLetter, restOfPot) word =
    if String.length word > 3 then
        if String.contains (String.fromChar keyLetter) word then
            checkLetters (keyLetter :: restOfPot) word

        else
            False

    else
        False


findPots =
    Http.get
        { url = "/pots.txt"
        , expect = Http.expectString FoundPots
        }


findWords pot =
    Http.get
        { url = "/dictionary.txt"
        , expect = Http.expectString (FoundWords pot)
        }


setToast string model =
    case model.state of
        Loaded honeycomb ->
            (honeycomb, Cmd.none)
                |> Toasty.addToast toastyConfig ToastyMsg (Toasty.Defaults.Warning string "")
                |> (\(h, c) -> ({ model | state = Loaded h }, c))

        _ ->
            (model, Cmd.none)


shuffleLetters otherLetters =
    Random.List.shuffle otherLetters
        |> Random.generate LettersShuffled



--clearToast =
--    Task.perform (\_ -> ToastTimedOut) (Process.sleep 10000)



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            route model.key (potFromUrl url)

        FoundPots (Ok str) ->
            let
                pots = String.lines str
                    
            in
            ({ model | state = ChoosingPot }, choosePot pots)
            
        FoundPots (Err err) ->
            ({ model | state = Problem "problem finding pots" }, Cmd.none)

        ChosenPot maybePot ->
            case maybePot of
                Just pot ->
                    ({ model | state = ChoosingKeyLetter }, chooseKeyLetter pot)

                Nothing ->
                    ({ model | state = Problem "problem choosing pot" }, Cmd.none)

        ChosenKeyLetter (maybeKeyLetter, otherLetters) ->
            case maybeKeyLetter of
                Just kl ->
                    ( { model | state = FindingWords }
                    , Nav.pushUrl model.key ("/" ++ String.fromChar kl ++ String.fromList otherLetters)
                    )

                Nothing ->
                    ( { model | state = Problem "problem choosing key letter" }
                    , Cmd.none
                    )

        FoundWords ((keyLetter, restOfPot) as pot) (Ok str) ->
            let
                words = String.lines str
                possibleWords = List.filter (wordIsPossible pot) words
                    
            in
            ( { model | state = CheckingStorage }
            , checkInStorage (storageCheckEncoder keyLetter restOfPot possibleWords)
            )
            
        FoundWords _ (Err _) ->
            ({ model | state = Problem "problem finding words" }, Cmd.none)


        FoundInStorage (Ok { keyLetter, otherLetters, possibleWords, guesses }) ->
            ( { model | state =
                Loaded
                    { input = []
                    , keyLetter = keyLetter
                    , otherLetters = otherLetters
                    , possibleWords = possibleWords
                    , guesses = guesses
                    , wordListExpanded = False
                    , toasties = Toasty.initialState
                    }
              }
            , Cmd.none)
            
            
        FoundInStorage (Err _) ->
            ({ model | state = Problem "problem getting pot out of storage" }, Cmd.none)

        LetterClicked letter ->
            case model.state of
                Loaded honeycomb ->
                    ( { model | state = Loaded { honeycomb | input = letter :: honeycomb.input, wordListExpanded = False } }
                    , Cmd.none
                    )

                _ ->
                    (model, Cmd.none)

        LetterRemoved ->
            case model.state of
                Loaded honeycomb ->
                    ( { model | state = Loaded { honeycomb | input = List.drop 1 honeycomb.input, wordListExpanded = False } }
                    , Cmd.none
                    )

                _ ->
                    (model, Cmd.none)

        GuessMade ->
            case model.state of
                Loaded honeycomb ->
                    let
                        guess = String.fromList <| List.reverse honeycomb.input
                        wordListMinimised = { honeycomb | wordListExpanded = False } 
                            
                    in
                    if String.length guess <= 3 then
                        setToast "Word is too short" { model | state = Loaded wordListMinimised }
                    else if not <| List.member honeycomb.keyLetter honeycomb.input then
                        setToast "Word doesn't contain key letter" { model | state = Loaded wordListMinimised }
                    else if List.member guess honeycomb.guesses then
                        setToast "Word already guessed" { model | state = Loaded wordListMinimised }
                    else if not <| List.member guess honeycomb.possibleWords then
                        setToast "Word not in answer set" { model | state = Loaded wordListMinimised }
                    else
                        ( { model | state = Loaded { wordListMinimised | input = [], guesses = guess :: honeycomb.guesses } }
                        , Cmd.batch
                          [ putInStorage (storageEncoder honeycomb.keyLetter honeycomb.otherLetters <| guess :: honeycomb.guesses)
                          , Cmd.none
                          ]
                        )

                _ ->
                    (model, Cmd.none)

        ShuffleLetters ->
            case model.state of
                Loaded honeycomb ->
                    ( { model | state = Loaded { honeycomb | wordListExpanded = False } }
                    , Cmd.batch
                        [ shuffleLetters honeycomb.otherLetters
                        , Cmd.none
                        ]
                    )

                _ ->
                    (model, Cmd.none)

        LettersShuffled otherLetters ->
            case model.state of
                Loaded honeycomb ->
                    ( { model | state = Loaded { honeycomb | otherLetters = otherLetters, input = [] } }
                    , Cmd.none
                    )

                _ ->
                    (model, Cmd.none)

        ShuffleHoneycomb ->
            ({ model | state = FindingPots }, findPots)

        WordListDropdownToggled ->
            case model.state of
                Loaded honeycomb ->
                    ( { model | state = Loaded { honeycomb | wordListExpanded = not honeycomb.wordListExpanded } }
                    , if not honeycomb.wordListExpanded then
                        instantScrollToBottom "#word-list-scroller"
                      else
                        Cmd.none
                    )

                _ ->
                    (model, Cmd.none)

        ToastyMsg subMsg ->
            case model.state of
                Loaded honeycomb ->
                    Toasty.update toastyConfig ToastyMsg subMsg honeycomb
                        |> (\(h, c) -> ({ model | state = Loaded h}, c))

                _ ->
                    (model, Cmd.none)


potFromUrl : Url.Url -> Maybe (Char, String)
potFromUrl url =
    let
        map =
            Parser.oneOf
                [ Parser.map Nothing Parser.top
                , Parser.map Just Parser.string
                ]

        turnIntoPot str =
            let
                set =
                    String.toList str
                        |> Set.fromList

                maybePot = String.uncons str

            in
                if Set.size set == 7 then
                    maybePot

                else
                    Nothing

    in
        Parser.parse map url
            |> Maybe.andThen (Maybe.andThen turnIntoPot)



route key maybePot =
    case maybePot of
        Just (kl, rest) ->
            ({ key = key, state = FindingWords }, findWords (kl, String.toList rest))

        Nothing ->
            ({ key = key, state = FindingPots }, findPots)



view : Model -> Browser.Document Msg
view model =
    { title = "🌸"
    , body =
        case model.state of
            FindingPots ->
                [ Html.div [] [ Html.text "finding pots (:" ] ]

            ChoosingPot ->
                [ Html.div [] [ Html.text "choosing pot" ] ]

            ChoosingKeyLetter ->
                [ Html.div [] [ Html.text "choosing key letter" ] ]

            FindingWords ->
                [ Html.div [] [ Html.text "finding words" ] ]

            CheckingStorage ->
                [ Html.div [] [ Html.text "checking in storage" ] ]

            Loaded honeycomb ->
                let
                    details = progressDetails honeycomb

                in
                [ Html.div
                    [ Attr.class "flex flex-col content-center min-h-full"
                    , Attr.attribute "style" ("--current-colour: " ++ Color.toCssString details.colour)
                    ]
                    [ Html.div [ Attr.class "relative z-50" ] [ Toasty.view toastyConfig Toasty.Defaults.view ToastyMsg honeycomb.toasties ]
                    , statusBar details honeycomb
                    , progressBar details
                    , Html.div
                        [ Attr.class "flex flex-col-reverse flex-grow md:flex-row my-8 mx-4 lg:px-16 justify-end md:justify-center overflow-hidden"
                        ]
                        [ honey details honeycomb
                        , wordList honeycomb.guesses honeycomb.wordListExpanded
                        ]
                    ]
                ]

            Problem problem ->
                [ Html.div [] [ Html.text ("problem: " ++ problem) ] ]
    }


renderToast toast =
    Html.div [ Attr.class "pt-4" ]
        [ Html.div [ Attr.class "text-gray-100 bg-gray-900 font-medium border-b-1 py-2 px-4" ]
            [ Html.text toast ]
        ] 


--toastBar toasts =
--    let
--        toastDiv toast =
            
---- toastDiv "Testing toast" :: toastDiv "toast 2" :: 
--    in
--    Html.div [ Attr.class "toasts z-20 sticky w-full top-0 left-0 flex flex-col items-center" ]
--        (List.map toastDiv (List.reverse <| List.take 3 <| List.reverse toasts))


honey details honeycomb =
    Html.div [ Attr.class ("lg:mx-16 md:mx-8") ]
        [ Html.div [ Attr.class "flex justify-center sm:text-5xl text-4xl tracking-wide my-8" ]
            (inputComponent details.colour honeycomb.keyLetter (List.reverse honeycomb.input))
        , Html.div [ Attr.class "hex-container" ]
            (honeycombComponent details.colour honeycomb)
        , Html.div [ Attr.class "flex justify-center my-8" ]
            [ Html.button
                [ Attr.class "p-2 mr-4 border border-black border-1 rounded-full"
                , Event.onClick ShuffleLetters
                ]
                [ Icon.Refresh.render ]
            , Html.button
                [ Attr.class "py-2 px-3 mr-4 leading-none border border-black border-1 rounded-md"
                , Event.onClick LetterRemoved
                ]
                [ Html.text "Delete" ]
            , Html.button
                [ Attr.class "py-2 px-3 leading-none border border-black border-1 rounded-md"
                , Event.onClick GuessMade
                ]
                [ Html.text "Enter" ]
            ]
        ]


wordList guesses wordListExpanded =
    let
        classes =
            if wordListExpanded then
                "bottom-full translate-y-full md:translate-y-0 flex-col overflow-scroll"
            else
                "bottom-0 translate-y-0 flex-col-reverse overflow-hidden"

        placeholder =
            Html.li [  ] [ Html.text "\u{200B}" ]

        wordSpan word =
            Html.li [ Attr.class "mx-3 inline-block" ] [ Html.text word ]

        chevronClass =
            if wordListExpanded then
                ""

            else
                "rotate-180"

    in 
    Html.div [ Attr.class ("lg:mx-16 md:mx-8 w-full md:w-4/12 text-sm") ]
        [ Html.div [ Attr.class "z-10 relative flex justify-between items-center p-4 bg-gray-100 rounded-md border border-1 rounded-b-none" ]
            [ Html.text ("You have found " ++ (String.fromInt <| List.length guesses) ++ " words")
            , Html.span [ Event.onClick WordListDropdownToggled, Attr.class ("md:hidden transition-all duration-1000 ease-in-out transform " ++ chevronClass) ] [ Icon.ChevronDown.render ]
            ] 
        , Html.div [ Attr.class ("p-4 md:p-0 relative leading-loose") ]
            [ Html.span [ Attr.class "md:hidden" ] [ placeholder ]
            , Html.div
                [ Attr.id "word-list-scroller", Attr.class ("wordlist-wrapper flex absolute md:static text-base w-full max-h-half-screen md:max-h-full left-0 bg-gray-100 rounded-md border border-1 border-t-0 rounded-t-none transition-all duration-1000 ease-in-out transform " ++ classes) ]
                [ Html.ul [ Attr.class "wordlist p-4 pr-6 flex flex-row-reverse flex-wrap-reverse justify-between leading-loose h-full" ]
                    (placeholder :: List.map wordSpan guesses)
                ]
            ]
        ]


statusBar details honeycomb =
    Html.div [ Attr.class "flex justify-between border border-1 text-xs items-center py-2 px-4" ]
        [ Html.div [ Attr.class "text-sm" ]
            [ Html.span [ Attr.class "font-medium mr-4" ] [ Html.text (String.fromList <| (honeycomb.keyLetter :: List.sort honeycomb.otherLetters)) ]
            , Html.span [] [ Html.text ((String.fromInt <| List.length honeycomb.possibleWords) ++ " possible words")]
            ]
        , Html.button
            [ Attr.class "py-2 px-3 leading-none border border-black border-1 rounded-md"
            , Event.onClick ShuffleHoneycomb
            ]
            [ Html.text "Shuffle" ] 
        ]


achievementPercentages =
    [ ("Beginner", 0)
    , ("Good start", 0.01)
    , ("Moving up", 0.05)
    , ("Good", 0.08)
    , ("Solid", 0.15)
    , ("Nice", 0.25)
    , ("Great", 0.4)
    , ("Amazing", 0.5)
    , ("Genius", 0.7)
    ]


findAchievement : Int -> List (String, Int) -> String
findAchievement currentScore achievements =
    case achievements of
        (xl, xn) :: (yl, yn) :: xs ->
            if currentScore >= xn && currentScore < yn then
                xl

            else
                findAchievement currentScore <| (yl, yn) :: xs

        _ ->
            "Genius"


pairPercentages pairs achievements =
    case achievements of
        (xl, xn) :: (yl, yn) :: xs ->
            pairPercentages ((xn, yn) :: pairs) ((yl, yn) :: xs)

        _ ->
            List.reverse pairs



wordToScore word =
    case String.length word of
        4 ->
            1

        length ->
            case Set.size <| Set.fromList <| String.toList word of
                7 ->
                    length + 7

                _ ->
                    length


progressDetails honeycomb =
    let
        highestScore =
            List.map wordToScore honeycomb.possibleWords
                |> List.sum

        currentScore =
            List.map wordToScore honeycomb.guesses
                |> List.sum

        achievementScore : Float -> Int
        achievementScore percentage =
            ceiling (toFloat highestScore * percentage)

        achievement =
            List.map (\(l, n) -> (l, achievementScore n)) achievementPercentages
                |> findAchievement currentScore

        colour = findColour (toFloat currentScore) (toFloat highestScore)

    in
    { colour = colour
    , achievement = achievement
    , highestScore = highestScore
    , currentScore = currentScore
    }


progressBar details =
    let
        segment (minPercent, maxPercent) =
            let
                -- Values
                minScore = ceiling (toFloat details.highestScore * minPercent)
                maxScore = ceiling (toFloat details.highestScore * maxPercent)
                segmentRange = maxScore - minScore

                -- Generated Attributes
                gridColumns = Attr.style "grid-template-columns" ("repeat(" ++ (String.fromInt <| segmentRange) ++ ", minmax(0, 1fr))")

                columnSpan = 
                    if details.currentScore > minScore then
                        (String.fromInt <| Basics.min segmentRange (details.currentScore - minScore))
                            |> (\n -> Attr.style "grid-column" ("span " ++ n ++ " / span " ++ n))

                    else
                        Attr.class "hidden"

                backgroundGradient = Attr.style "background" ("linear-gradient(.25turn, " ++ (Color.toCssString <| findColour (toFloat minScore) (toFloat details.highestScore)) ++ "," ++ (Color.toCssString <| findColour (toFloat (min maxScore details.currentScore)) (toFloat details.highestScore)) ++ ")")

            in
                Html.div
                    [ Attr.class "bg-gray-300 h-1 grid rounded-sm"
                    , gridColumns
                    ]
                    [ Html.div
                        [ Attr.class "progress-indicator bg-green-400 h-1 rounded-sm "
                        , backgroundGradient
                        , columnSpan
                        ]
                        []
                    ]

        segments =
            pairPercentages [] achievementPercentages
                |> List.map segment

            
    in
    Html.div [ Attr.class "flex px-4 sticky top-0 py-4 bg-white z-20 bg-gray-100" ]
        [ Html.div [ Attr.class "font-medium mr-2" ]
                [ Html.text details.achievement ] 
        , Html.div
            [ Attr.class "mr-2 rounded-md flex"
            , Attr.style "backgroundColor" (Color.toCssString details.colour)
            ]
            [ Html.div [ Attr.class "px-2 flex items-center justify-center bg-white text-xs leading-none tracking-wide text-black bg-opacity-50" ]
                [ Html.text (String.fromInt details.currentScore) ]
            ]
        , Html.div [ Attr.class "ml-2 grid grid-cols-8 gap-1 items-center flex-grow" ]
            segments
        ]


inputComponent colour keyLetter input =
    let
        textColour l =
            if l == keyLetter then
                Attr.style "color" (Color.toCssString colour)

            else
                Attr.style "" ""

        letter l =
            Html.div [ Attr.class "letter", textColour l ]
                [ Html.text (String.fromChar l) ]
            
    in
    (letter '\u{200B}') :: (List.map letter input)


findColour value max =
    (value / max)
        |> Scale.viridisInterpolator


honeycombComponent colour honeycomb =
    let
        hexagon letter isKey =
            Html.div
                [ Attr.classList [("hexagon font-sans font-medium select-none", True), ("hex-key", isKey)]
                , Event.onClick (LetterClicked letter)
                ]
                [ Html.div [ Attr.class "hex-letter" ] [ Html.text <| String.fromChar letter ] ]
            
    in  
    case (honeycomb.keyLetter, honeycomb.otherLetters) of
        (kl, [l1, l2, l3, l4, l5, l6]) ->
            [ Html.div [ Attr.class "hex-row" ]
                [ hexagon l1 False, hexagon l2 False ]
            , Html.div [ Attr.class "hex-row" ]
                [ hexagon l3 False, hexagon kl True, hexagon l4 False ]
            , Html.div [ Attr.class "hex-row" ]
                [ hexagon l5 False, hexagon l6 False ]
            ]


        _ ->
            [ Html.text "problem displaying honeycomb" ]


storageCheckEncoder keyLetter otherLetters possibleWords =
    Encoder.object
        [ ("keyLetter", Encoder.string <| String.fromChar keyLetter)
        , ("otherLetters", Encoder.list Encoder.string <| List.map String.fromChar otherLetters)
        , ("possibleWords", Encoder.list Encoder.string possibleWords)
        ]


type alias StorageCheckResult =
    { keyLetter : Char
    , otherLetters : List Char
    , possibleWords : List String
    , guesses : List String
    }


charDecoder =
    let
        charHelp str =
            case String.uncons str of
                Just (s, "") ->
                    Decoder.succeed s

                _ ->
                    Decoder.fail "Not a char"
    in  
    Decoder.string
        |> Decoder.andThen charHelp


storageCheckDecoder =
    Decoder.map4 StorageCheckResult
        (Decoder.field "keyLetter" charDecoder)
        (Decoder.field "otherLetters" <| Decoder.list charDecoder)
        (Decoder.field "possibleWords" <| Decoder.list Decoder.string)
        (Decoder.field "guesses" <| Decoder.list Decoder.string)


storageEncoder keyLetter otherLetters guesses =
    Encoder.object
        [ ("keyLetter", Encoder.string <| String.fromChar keyLetter)
        , ("otherLetters", Encoder.list Encoder.string <| List.map String.fromChar otherLetters)
        , ("guesses", Encoder.list Encoder.string <| guesses)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    foundInStorage (FoundInStorage << Decoder.decodeValue storageCheckDecoder)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
