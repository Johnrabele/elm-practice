module Todos exposing (Todo, Todos, changeText, id, isEditing, text)


type alias Todo =
    ( Int, String )


type alias Todos =
    List Todo


id : Todo -> Int
id =
    Tuple.first


text : Todo -> String
text =
    Tuple.second


isEditing : Maybe Todo -> Todo -> Bool
isEditing selected todo =
    let
        selectedId =
            selected |> Maybe.map Tuple.first
    in
    selectedId == Just (Tuple.first todo)


changeText : Todo -> String -> Todo
changeText ( idT, value) string =
    ( idT, string )