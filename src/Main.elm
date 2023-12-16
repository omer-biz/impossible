module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Time


type alias Question =
    { question : String, answer : Maybe String }


type History
    = History
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] <|
        History
            { previous = [ Question "Why ?" Nothing ]
            , current = Question "How are you feeling today ?" Nothing
            , next = [ Question "What do you think about ?" Nothing ]
            }
    , Cmd.none
    )


nextQuestion : History -> History
nextQuestion (History history) =
    case history.next of
        [] ->
            History history

        newNext :: rest ->
            History
                { previous = history.current :: history.previous
                , current = newNext
                , next = rest
                }


prevQuestion : History -> History
prevQuestion (History history) =
    case history.previous of
        [] ->
            History history

        newPrev :: rest ->
            History
                { previous = rest
                , current = newPrev
                , next = history.current :: history.next
                }


lenQuestions : History -> Int
lenQuestions (History { previous, next }) =
    List.length previous + List.length next + 1


deleteCurrentQuestion : History -> ( Question, History )
deleteCurrentQuestion (History { previous, current, next }) =
    case ( previous, next ) of
        ( [], [] ) ->
            ( current, History { previous = previous, current = current, next = next } )

        ( newCurr :: restPrev, restNext ) ->
            ( current, History { previous = restPrev, current = newCurr, next = restNext } )

        ( restPrev, newCurr :: restNext ) ->
            ( current, History { previous = restPrev, current = newCurr, next = restNext } )


addQuestion : Question -> History -> History
addQuestion question (History history) =
    History { history | previous = history.current :: history.previous, current = question }


removeFromList : Int -> List a -> List a
removeFromList i xs =
    List.take i xs ++ List.drop (i + 1) xs


statusDelay : number
statusDelay =
    11


statusWithTimer : StatusState -> Status
statusWithTimer =
    Status statusDelay


decrementByOne : List Status -> List Status
decrementByOne statuses =
    let
        minusOne status =
            { status | timer = status.timer - 1 }
    in
    statuses
        |> List.map minusOne
        |> List.filter (\stat -> stat.timer > 0)


nothing : Html msg
nothing =
    text ""


view : Model -> Html Msg
view model =
    div [] [ viewStatuses model.status, viewHistory model.questions ]


viewStatuses : List Status -> Html Msg
viewStatuses status =
    let
        viewStatus statusIndex stat =
            case stat.state of
                Notification message ->
                    div [ onClick <| RemoveStatus statusIndex ] [ text message, text <| String.fromInt stat.timer ]

                TextDeleted message question ->
                    div [ onClick <| RemoveStatus statusIndex ]
                        [ text message
                        , button [ onClick <| UndoDeleteAndRemove question statusIndex ] [ text "Undo" ]
                        , text <| String.fromInt stat.timer
                        ]
    in
    div []
        (status
            |> List.indexedMap viewStatus
        )


viewHistory : History -> Html Msg
viewHistory (History history) =
    let
        nextButton =
            if List.length history.next > 0 then
                button [ onClick NextQuestion ] [ text "Next" ]

            else
                nothing

        prevButton =
            if List.length history.previous > 0 then
                button [ onClick PreviousQuestion ] [ text "Prev" ]

            else
                nothing
    in
    div [] [ prevButton, viewQuestion history.current, nextButton ]


viewQuestion : Question -> Html Msg
viewQuestion question =
    let
        answerValue =
            case question.answer of
                Just answer ->
                    answer

                Nothing ->
                    ""
    in
    div []
        [ text question.question
        , input [ onInput ChangeQuestion, value answerValue ] []
        , button [ onClick SaveQuestion ] [ text "Save" ]
        , button [ onClick DeleteQuestion ] [ text "Delete" ]
        ]


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
            ( { model
                | status = decrementByOne model.status
              }
            , Cmd.none
            )

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


updateCurrentQuestion : History -> String -> History
updateCurrentQuestion (History history) input =
    History { history | current = updateAnswer history.current input }


updateAnswer : Question -> String -> Question
updateAnswer question answer =
    { question | answer = Just answer }


subscriptions : Model -> Sub Msg
subscriptions model =
    if List.length model.status > 0 then
        Time.every 1000 Tick

    else
        Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
