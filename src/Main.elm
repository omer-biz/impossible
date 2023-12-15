module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, span, text, time)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Process
import Task


type alias Question =
    { question : String, answer : Maybe String }


type History
    = History
        { previous : List Question
        , current : Question
        , next : List Question
        }


type Status
    = NoStatus
    | Notification String
    | TextDeleted String Question


type alias Model =
    { status : Status, questions : History }


type Msg
    = ChangeQuestion String
    | SaveQuestion
    | DeleteQuestion
    | NextQuestion
    | PreviousQuestion
    | TimerEnded
    | UndoDelete Question


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NoStatus <|
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
    List.length previous
        |> (+) (List.length next)
        |> (+) 1


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


view : Model -> Html Msg
view model =
    div [] [ viewStatus model.status, viewHistory model.questions ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        NoStatus ->
            div [] []

        Notification message ->
            div [] [ text message ]

        TextDeleted message question ->
            div []
                [ text message
                , button [ onClick <| UndoDelete question ] [ text "Undo" ]
                ]


viewHistory : History -> Html Msg
viewHistory (History history) =
    let
        nextButton =
            if List.length history.next > 0 then
                button [ onClick NextQuestion ] [ text "Next" ]

            else
                span [] []

        prevButton =
            if List.length history.previous > 0 then
                button [ onClick PreviousQuestion ] [ text "Prev" ]

            else
                span [] []
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
            ( { model | status = Notification "Question:  Saved" }, startTimer TimerEnded 10000 )

        NextQuestion ->
            ( { model | questions = nextQuestion model.questions }, Cmd.none )

        PreviousQuestion ->
            ( { model | questions = prevQuestion model.questions }, Cmd.none )

        ChangeQuestion input ->
            ( { model | questions = updateCurrentQuestion model.questions input }, Cmd.none )

        TimerEnded ->
            ( { model | status = NoStatus }, Cmd.none )

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
            ( { model | status = status, questions = Tuple.second deleteQue }
            , startTimer TimerEnded 10000
            )

        UndoDelete question ->
            ( { model
                | status = Notification "Question: Restored"
                , questions = addQuestion question model.questions
              }
            , startTimer TimerEnded 10000
            )


startTimer : Msg -> Float -> Cmd Msg
startTimer msg time =
    Process.sleep time
        |> Task.perform (\_ -> msg)


updateCurrentQuestion : History -> String -> History
updateCurrentQuestion (History history) input =
    History { history | current = updateAnswer history.current input }


updateAnswer : Question -> String -> Question
updateAnswer question answer =
    { question | answer = Just answer }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
