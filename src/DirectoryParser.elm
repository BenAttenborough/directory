module DirectoryParser exposing (..)

import Browser
import Directory exposing (..)
import Html exposing (Html, button, div, input, p, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import Parser exposing (..)
import Tree.Zipper as Zipper


type alias Model =
    { terminalInput : String
    , directoryTree : Zipper.Zipper Directory
    , terminalOutput : List String
    , commandBuffer : List String
    }


initialModel : Model
initialModel =
    { terminalInput = ""
    , directoryTree = singleton (Directory "root" [])
    , terminalOutput = []
    , commandBuffer = []
    }


type Msg
    = OnChange String
    | OnKeyDown Int


view : Model -> Html Msg
view { terminalInput, directoryTree, terminalOutput } =
    Html.div [ class "main-container" ]
        [ div []
            [ div [] [ text "Directory tree" ]
            , directoryTree |> toHtml
            ]
        , div []
            [ div [ class "terminal-output" ]
                [ p [] [ text "ELM 9000" ]
                , div []
                    (List.map
                        (\line ->
                            p [ class "command-line" ] [ text line ]
                        )
                        terminalOutput
                    )
                , div
                    [ style "display" "grid"
                    , style "grid-template-columns" "auto 1fr"
                    , style "margin-top" "-5px"
                    ]
                    [ p [] [ text (terminalPrompt directoryTree) ]
                    , input
                        [ placeholder "Type your command"
                        , value terminalInput
                        , onInput OnChange
                        , onKeyDown OnKeyDown
                        , style "margin-left" "10px"
                        ]
                        []
                    ]
                ]
            ]
        ]


terminalPrompt : Zipper.Zipper Directory -> String
terminalPrompt directory =
    " $ "
        |> String.append (getLabelsRecursively [] directory)


modelUpdater : Maybe (Zipper.Zipper Directory) -> List String -> Model -> Model
modelUpdater directory terminalOutput model =
    let
        terminalInputOutput =
            (terminalPrompt model.directoryTree ++ model.terminalInput) :: terminalOutput

        newModel =
            { model
                | terminalOutput = List.append model.terminalOutput terminalInputOutput
                , terminalInput = ""
                , commandBuffer = model.terminalInput :: model.commandBuffer
            }
    in
    case directory of
        Just modifiedDirectory ->
            { newModel
                | directoryTree = modifiedDirectory
            }

        Nothing ->
            newModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnChange terminalInput ->
            { model | terminalInput = terminalInput }

        OnKeyDown key ->
            if key == 13 then
                let
                    parserResult =
                        Parser.run commandParser model.terminalInput
                in
                case parserResult of
                    Ok command ->
                        case command of
                            CD directoryName ->
                                case changeDirectoryCommand directoryName model.directoryTree of
                                    Ok value ->
                                        modelUpdater (Just value) [ "Change directory: " ++ directoryName ] model

                                    Err err ->
                                        modelUpdater Nothing [ err ] model

                            LS ->
                                modelUpdater Nothing (listDir model.directoryTree) model

                            MakeDir name ->
                                case addFolderCommand (Directory name []) model.directoryTree of
                                    Ok val ->
                                        modelUpdater (Just val) [ "Made directory: " ++ name ] model

                                    Err err ->
                                        modelUpdater Nothing [ err ] model

                            Touch fileName fileSize ->
                                modelUpdater
                                    (Just (addFile (File fileName fileSize) model.directoryTree))
                                    [ "Created file" ]
                                    model

                            Clear ->
                                modelUpdater Nothing [] model

                            Pwd ->
                                modelUpdater
                                    Nothing
                                    [ getLabelsRecursively [] model.directoryTree ]
                                    model

                    Err error ->
                        modelUpdater Nothing [ "Error: " ++ Parser.deadEndsToString error ] model

            else if key == 38 then
                -- up key
                case model.commandBuffer of
                    [] ->
                        model

                    command :: _ ->
                        { model
                            | terminalInput = command
                        }

            else
                model


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


type Command
    = CD String
    | LS
    | MakeDir String
    | Touch String Int
    | Clear
    | Pwd


word : Parser String
word =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c)
            |. chompWhile (\c -> Char.isAlphaNum c || c == '.')


dirWord : Parser String
dirWord =
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlphaNum c || c == '.')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '.')


commandParser : Parser Command
commandParser =
    succeed identity
        |= oneOf
            [ succeed CD
                |. keyword "cd"
                |. spaces
                |= dirWord
            , succeed LS
                |. keyword "ls"
            , succeed MakeDir
                |. keyword "mkdir"
                |. spaces
                |= word
            , succeed Touch
                |. keyword "touch"
                |. spaces
                |= word
                |. spaces
                |= int
            , succeed Clear
                |. keyword "clear"
            , succeed Pwd
                |. keyword "pwd"
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
