module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (roundEach)
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Time



-- Model


type alias Question =
    { question : String, answer : Maybe String }


type alias History =
    { previous : List Question
    , current : Question
    , next : List Question
    }


type StatusState
    = Notification String
    | TextDeleted String Question


type alias Status =
    { timer : Int
    , state : StatusState
    }


type alias Model =
    { status : List Status, questions : History }


type Msg
    = ChangeQuestion String
    | SaveQuestion
    | DeleteQuestion
    | NextQuestion
    | PreviousQuestion
    | Tick Time.Posix
    | UndoDeleteAndRemove Question Int
    | RemoveStatus Int
    | ViewAtIndex Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] <|
        { previous = [ Question "Why ?" Nothing, Question "What is .. ?" Nothing ]
        , current = Question "How are you feeling today ?" Nothing
        , next = [ Question "What are you thinking about ?" Nothing, Question "And yet another question ?" Nothing ]
        }
    , Cmd.none
    )



-- History Ops


nextQuestion : History -> History
nextQuestion history =
    case history.next of
        [] ->
            history

        newNext :: rest ->
            { previous = history.current :: history.previous
            , current = newNext
            , next = rest
            }


prevQuestion : History -> History
prevQuestion history =
    case history.previous of
        [] ->
            history

        newPrev :: rest ->
            { previous = rest
            , current = newPrev
            , next = history.current :: history.next
            }


lenQuestions : History -> Int
lenQuestions { previous, next } =
    List.length previous + List.length next + 1


deleteCurrentQuestion : History -> ( Question, History )
deleteCurrentQuestion { previous, current, next } =
    case ( previous, next ) of
        ( [], [] ) ->
            ( current, { previous = [], current = current, next = [] } )

        ( newCurr :: restPrev, restNext ) ->
            ( current, { previous = restPrev, current = newCurr, next = restNext } )

        ( restPrev, newCurr :: restNext ) ->
            ( current, { previous = restPrev, current = newCurr, next = restNext } )


addQuestion : Question -> History -> History
addQuestion question history =
    { history | previous = history.current :: history.previous, current = question }


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs


resetCurQuestion : Int -> History -> History
resetCurQuestion idx ({ previous } as history) =
    if idx < List.length previous then
        shiftPrevious idx history

    else if idx > List.length previous then
        shiftNext idx history

    else
        history


shiftPrevious : Int -> History -> History
shiftPrevious idx ({ previous, current, next } as history) =
    let
        revList =
            List.reverse previous
    in
    case List.drop idx revList of
        curr :: rest ->
            { history
                | previous = List.reverse <| List.take idx revList
                , current = curr
                , next = rest ++ current :: next
            }

        _ ->
            history


shiftNext : Int -> History -> History
shiftNext idx ({ previous, current, next } as history) =
    let
        newIdx =
            idx - List.length previous - 1
    in
    case List.drop newIdx next of
        curr :: rest ->
            { history
                | previous = current :: List.take newIdx next ++ previous
                , current = curr
                , next = rest
            }

        _ ->
            history



-- Status


decrementByOne : List Status -> List Status
decrementByOne statuses =
    statuses
        |> List.map (\status -> { status | timer = status.timer - 1 })
        |> List.filter (\stat -> stat.timer > 0)


statusDelay : number
statusDelay =
    10


statusWithTimer : StatusState -> Status
statusWithTimer =
    Status statusDelay



-- View


nothing : Element.Element Msg
nothing =
    text ""


view : Model -> Html Msg
view model =
    layout [ Background.color <| rgb255 0xEA 0xE5 0xD7 ] <|
        column
            [ width fill, height fill ]
            [ viewNotifications model.status, viewHistory model.questions ]


viewNotifications : List Status -> Element.Element Msg
viewNotifications status =
    let
        customCss key val =
            Element.htmlAttribute (Html.Attributes.style key val)

        viewNotification statusIndex stat =
            column statusStyle <|
                (el [ onClick <| RemoveStatus statusIndex, customCss "position" "absolute", alignRight, pointer ] <| text "✘")
                    :: (case stat.state of
                            Notification message ->
                                [ text message ]

                            TextDeleted message question ->
                                [ text message
                                , Input.button (alignRight :: btnUndoStyle) { onPress = Just <| UndoDeleteAndRemove question statusIndex, label = text "Undo" }
                                ]
                       )
                    ++ [ el [ alignRight, Font.size 11 ] (text <| String.fromInt stat.timer) ]
    in
    case status of
        [] ->
            nothing

        all ->
            all
                |> List.indexedMap viewNotification
                |> column
                    [ customCss "position" "fixed"
                    , customCss "z-index" "1"
                    , customCss "top" "5%"
                    , customCss "right" "5%"
                    , spacingXY 0 10
                    ]


viewHistory : History -> Element.Element Msg
viewHistory history =
    let
        nextButton =
            if List.length history.next > 0 then
                Input.button (alignRight :: sideNextStyle) { onPress = Just NextQuestion, label = text "Next" }

            else
                Input.button (alignRight :: disabledSideBtn) { onPress = Nothing, label = text "Next" }

        prevButton =
            if List.length history.previous > 0 then
                Input.button (alignLeft :: sidePrevStyle) { onPress = Just PreviousQuestion, label = text "Prev" }

            else
                Input.button (alignLeft :: disabledSideBtn) { onPress = Nothing, label = text "Prev" }
    in
    row [ width fill, {- explain Debug.todo, -} height fill ]
        [ prevButton
        , column [ centerX, width fill, height fill ]
            [ column [ centerX, centerY, spacing 20 ]
                [ viewQuestion history.current, viewControl <| lenQuestions history ]
            , viewPagination history
            ]
        , nextButton
        ]


viewPagination : History -> Element Msg
viewPagination history =
    let
        isActiveQue idx =
            if idx == List.length history.previous then
                selectedPageBtnStyle

            else
                pageBtnStyle

        pageBtn idx =
            Input.button (isActiveQue idx)
                { onPress = Just <| ViewAtIndex idx
                , label = text <| String.fromInt (idx + 1)
                }

        -- TODO: When it's no longer to show all the number of questions
        -- add `next`` and `prev` buttons
    in
    row [ alignBottom, centerX, spacingXY 15 0, paddingXY 0 30 ]
        (List.range 0 (lenQuestions history - 1)
            |> List.map pageBtn
        )


viewControl : Int -> Element.Element Msg
viewControl qLen =
    let
        deleteButton =
            if qLen /= 1 then
                Input.button btnDelStyle { onPress = Just DeleteQuestion, label = text "Delete" }

            else
                nothing
    in
    row [ centerX, spacing 12 ]
        [ Input.button btnStyle { onPress = Just SaveQuestion, label = text "Save" }
        , deleteButton
        ]


viewQuestion : Question -> Element.Element Msg
viewQuestion question =
    column [ spacing 20 ]
        [ el [ centerX ] <| text question.question
        , Input.text
            ([ width <| px 250, centerX ] ++ inputStyle)
            { onChange = ChangeQuestion
            , text = Maybe.withDefault "" question.answer
            , placeholder = Just <| Input.placeholder [] (text "Your response here")
            , label = Input.labelHidden "Answer"
            }
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveQuestion ->
            ( { model | status = (statusWithTimer <| Notification "Question:  Saved") :: model.status }, Cmd.none )

        NextQuestion ->
            ( { model | questions = nextQuestion model.questions }, Cmd.none )

        PreviousQuestion ->
            ( { model | questions = prevQuestion model.questions }, Cmd.none )

        ChangeQuestion input ->
            ( { model | questions = updateCurrentQuestion model.questions input }, Cmd.none )

        Tick _ ->
            ( { model | status = decrementByOne model.status }, Cmd.none )

        RemoveStatus index ->
            ( { model | status = removeFromList index model.status }, Cmd.none )

        DeleteQuestion ->
            let
                deleteQue =
                    deleteCurrentQuestion model.questions

                status =
                    if lenQuestions model.questions == 1 then
                        Notification "Can't Delete the last questions"

                    else
                        TextDeleted "Question: Deleted" <| Tuple.first deleteQue
            in
            ( { model | status = (statusWithTimer <| status) :: model.status, questions = Tuple.second deleteQue }
            , Cmd.none
            )

        UndoDeleteAndRemove question index ->
            ( { model
                | status = (statusWithTimer <| Notification "Question: Restored") :: removeFromList index model.status
                , questions = addQuestion question model.questions
              }
            , Cmd.none
            )

        ViewAtIndex idx ->
            ( { model | questions = resetCurQuestion (Debug.log "idx" idx) model.questions }, Cmd.none )


updateCurrentQuestion : History -> String -> History
updateCurrentQuestion history input =
    { history | current = updateAnswer history.current input }


updateAnswer : Question -> String -> Question
updateAnswer question answer =
    { question | answer = Just answer }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.length model.status > 0 then
        Time.every 1000 Tick

    else
        Sub.none



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Style


sidePrevStyle : List (Element.Attribute msg)
sidePrevStyle =
    [ padding 10
    , Background.color <| purple Soft
    , Border.width 2
    , roundEach { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 }
    , height fill
    , mouseDown [ Background.color <| purple Hard ]
    , mouseOver [ Background.color <| purple Medium ]
    ]


sideNextStyle : List (Element.Attribute msg)
sideNextStyle =
    [ padding 10
    , Background.color <| cyan Soft
    , Border.width 2
    , roundEach { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 }
    , height fill
    , mouseDown [ Background.color <| cyan Hard ]
    , mouseOver [ Background.color <| cyan Medium ]
    ]


disabledSideBtn : List (Element.Attribute msg)
disabledSideBtn =
    [ padding 10
    , Background.color <| grey Soft
    , Border.width 2
    , roundEach { topLeft = 5, topRight = 0, bottomLeft = 5, bottomRight = 0 }
    , height fill
    ]


inputStyle : List (Element.Attribute msg)
inputStyle =
    [ padding 10
    , focused [ Background.color <| pink Soft, borderShadow ]
    , Border.width 2
    , Border.color <| black Hard
    , Font.size 15
    ]


borderShadow : Element.Attr decorative msg
borderShadow =
    Border.shadow { offset = ( 2, 2 ), color = rgba255 0x00 0x00 0x00 0x01, size = 1, blur = 0 }


btnStyle : List (Element.Attribute msg)
btnStyle =
    [ padding 7
    , Background.color <| green Soft
    , Border.width 2
    , borderShadow
    , mouseDown [ Background.color <| green Hard ]
    , mouseOver [ Background.color <| green Medium ]
    , focused [ borderShadow ]
    , Border.rounded 7
    ]


btnDelStyle : List (Element.Attribute msg)
btnDelStyle =
    [ padding 7
    , Background.color <| red Soft
    , Border.width 2
    , borderShadow
    , mouseDown [ Background.color <| red Hard ]
    , mouseOver [ Background.color <| red Medium ]
    , focused [ borderShadow ]
    , Border.rounded 7
    ]


btnUndoStyle : List (Element.Attribute msg)
btnUndoStyle =
    [ padding 7
    , Background.color <| pink Soft
    , Border.width 2
    , borderShadow
    , mouseDown [ Background.color <| pink Hard ]
    , mouseOver [ Background.color <| pink Medium ]
    , focused [ borderShadow ]
    , Border.rounded 7
    , Font.size 15
    ]


statusStyle : List (Element.Attribute msg)
statusStyle =
    [ Border.color <| black Hard
    , Background.color <| white Hard
    , borderShadow
    , Border.width 2
    , width <| px 220
    , spacingXY 10 15
    , paddingXY 10 5
    ]


pageBtnStyle : List (Attribute msg)
pageBtnStyle =
    [ Border.width 2
    , Background.color <| pink Soft
    , paddingXY 5 2
    , Border.rounded 7
    , Font.size 16
    , borderShadow
    , pointer
    , mouseDown [ Background.color <| pink Hard ]
    , mouseOver [ Background.color <| pink Medium ]
    , focused [ borderShadow ]
    ]


selectedPageBtnStyle : List (Attribute msg)
selectedPageBtnStyle =
    [ Border.width 2
    , Background.color <| cream Medium
    , paddingXY 5 2
    , Border.rounded 7
    , Font.size 16
    , borderShadow
    , focused [ borderShadow ]
    ]



-- Color


type Accent
    = Soft
    | Medium
    | Hard


color : Color -> Color -> Color -> Accent -> Color
color soft medium hard accent =
    case accent of
        Soft ->
            soft

        Medium ->
            medium

        Hard ->
            hard


pink : Accent -> Color
pink =
    color (rgb255 0xFF 0xA6 0xF6) (rgb255 0xFA 0x8C 0xEF) (rgb255 0xF7 0x74 0xEA)


red : Accent -> Color
red =
    color (rgb255 0xFF 0x55 0x55) (rgb255 0xE7 0x4C 0x3C) (rgb255 0xD9 0x53 0x4F)


green : Accent -> Color
green =
    color (rgb255 0xB8 0xFF 0x9F) (rgb255 0x9D 0xFC 0x7C) (rgb255 0x7D 0xF7 0x52)


black : Accent -> Color
black =
    color (rgb255 0x00 0x00 0x00) (rgb255 0x00 0x00 0x00) (rgb255 0x00 0x00 0x00)


grey : Accent -> Color
grey =
    color (rgb255 0xBF 0xBF 0xBF) (rgb255 0x00 0x00 0x00) (rgb255 0x00 0x00 0x00)


cyan : Accent -> Color
cyan =
    color (rgb255 0xA6 0xFA 0xFF) (rgb255 0x79 0xF7 0xFF) (rgb255 0x00 0xE1 0xEF)


purple : Accent -> Color
purple =
    color (rgb255 0xC4 0xA1 0xFF) (rgb255 0xA3 0x88 0xEE) (rgb255 0x97 0x23 0xC9)


cream : Accent -> Color
cream =
    color (rgb255 0xFD 0xFD 0x96) (rgb255 0xFF 0xD8 0x58) (rgb255 0xF4 0xD7 0x38)


white : Accent -> Color
white =
    color (rgb255 0xFF 0xFF 0xFF) (rgb255 0xFF 0xFF 0xFF) (rgb255 0xFF 0xFF 0xFF)
