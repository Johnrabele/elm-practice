module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Todos exposing (Todo, Todos)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , entries : List Todo
    , lastId : Int
    , selected : Maybe Todo
    }


init : Model
init =
    { content = ""
    , entries = []
    , lastId = 0
    , selected = Nothing
    }



-- UPDATE


type Msg
    = Change String
    | Add String
    | Edit Todo String
    | Update String


rename : List Todo -> Todo -> List Todo
rename list newTodo =
    list
        |> List.map
            (\todo ->
                if Tuple.first newTodo == Todos.id todo then
                    newTodo

                else
                    todo
            )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }

        Add new ->
            { model
                | content = ""
                , entries = ( model.lastId, new ) :: model.entries
                , lastId = model.lastId + 1
            }

        Edit todo string ->
            { model
                | selected = Just (Todos.changeText todo string)
            }

        Update _ ->
            { model | entries = Maybe.map (rename model.entries) model.selected |> Maybe.withDefault model.entries, selected = Nothing }



-- VIEW


viewEntry : Maybe Todo -> Todo -> Html Msg
viewEntry selected todo =
    let
        isEditing : Bool
        isEditing =
            Todos.isEditing selected todo

        editingAttributes : List (Html.Attribute Msg)
        editingAttributes =
            if isEditing then
                [ class "editing" ]

            else
                []

        editValue =
            selected |> Maybe.map Todos.text >> Maybe.withDefault (Todos.text todo)
    in
    li ([ onDoubleClick (Edit todo (Todos.text todo)) ] ++ editingAttributes)
        [ div [ class "view" ]
            [ label [] [ text (Todos.text todo) ]
            ]
        , input
            [ class "edit"
            , value editValue
            , onInput (Edit todo)
            , onEnter (Update editValue)
            ]
            []
        ]


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 []
                    [ text "todos" ]
                , input
                    [ onInput Change
                    , attribute "autofocus" ""
                    , class "new-todo"
                    , placeholder "What needs to be done?"
                    , value model.content
                    , onEnter (Add model.content)
                    ]
                    []
                ]
            , section [ class "main" ]
                [ input [ class "toggle-all", id "toggle-all", type_ "checkbox" ]
                    []
                , label [ for "toggle-all" ]
                    [ text "Mark all as complete" ]
                , ul [ class "todo-list" ]
                    (List.map (viewEntry model.selected) model.entries)
                ]
            , footer [ class "footer" ]
                []
            , footer
                [ class "info" ]
                [ p []
                    [ text "Double-click to edit a todo" ]
                , p []
                    [ text "Created by "
                    , a [ href "http://sindresorhus.com" ]
                        [ text "Sindre Sorhus" ]
                    ]
                , p []
                    [ text "Part of "
                    , a [ href "http://todomvc.com" ]
                        [ text "TodoMVC" ]
                    ]
                ]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)
