module Directory exposing
    ( Directory
    , DirectoryTree
    , File
    , addFile
    , addFiles
    , addFolder
    , addFolderCommand
    , changeDirectory
    , changeDirectoryCommand
    , getLabelsRecursively
    , listDir
    , printRoute
    , singleton
    , toHtml
    )

import Html exposing (Html)
import Tree exposing (Tree, tree)
import Tree.Zipper as Zipper


type alias Directory =
    { label : String
    , files : List File
    }


type alias File =
    { label : String
    , size : Int
    }


type DirectoryTree a
    = DirectoryTree Directory (List (DirectoryTree Directory))


singleton : Directory -> Zipper.Zipper Directory
singleton directory =
    Tree.singleton directory
        |> Zipper.fromTree


toHtml : Zipper.Zipper Directory -> Html msg
toHtml dir =
    dir
        |> Zipper.toTree
        |> Tree.restructure directoryToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


directoryToHtml : Directory -> Html msg
directoryToHtml dir =
    Html.div []
        [ Html.p []
            [ Html.text (dir.label ++ " (DIR)") ]
        , Html.div []
            [ dir.files
                |> List.map (\file -> Html.li [] [ Html.text (file.label ++ " (" ++ String.fromInt file.size ++ ")") ])
                |> Html.ul []
            ]
        ]


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]


addFolder : Directory -> Zipper.Zipper Directory -> Zipper.Zipper Directory
addFolder folder parent =
    parent
        |> Zipper.mapTree
            (addFolderInternal
                (tree folder [])
            )


childLabelConflictsWithExisting : Tree Directory -> List (Tree Directory) -> Bool
childLabelConflictsWithExisting child children =
    let
        childData =
            Tree.label child

        childrenLabels =
            List.map (\x -> Tree.label x) children
    in
    List.any (\item -> item.label == childData.label) childrenLabels


addFolderInternal : Tree.Tree Directory -> Tree.Tree Directory -> Tree.Tree Directory
addFolderInternal child parent =
    case Tree.children parent of
        [] ->
            Tree.replaceChildren
                [ child ]
                parent

        children ->
            if childLabelConflictsWithExisting child children then
                parent

            else
                Tree.prependChild
                    child
                    parent


addFolderCommand : Directory -> Zipper.Zipper Directory -> Result String (Zipper.Zipper Directory)
addFolderCommand folder parent =
    if childLabelConflictsWithExisting (tree folder []) (Tree.children (Zipper.toTree parent)) then
        Err ("mkdir: " ++ folder.label ++ ": File exists")

    else
        parent
            |> Zipper.mapTree
                (addFolderInternal
                    (tree folder [])
                )
            |> Ok


changeDirectory : String -> Zipper.Zipper Directory -> Zipper.Zipper Directory
changeDirectory needle haystack =
    let
        isNeedleInHaystack list =
            list
                |> List.any
                    (\child ->
                        let
                            data =
                                Tree.label child
                        in
                        data.label == needle
                    )
    in
    haystack
        |> Zipper.children
        |> (\children ->
                if isNeedleInHaystack children then
                    Zipper.findNext
                        (\x ->
                            x.label == needle
                        )
                        haystack
                        |> Maybe.withDefault haystack

                else
                    haystack
           )


changeDirectoryCommand : String -> Zipper.Zipper Directory -> Result String (Zipper.Zipper Directory)
changeDirectoryCommand needle haystack =
    if needle == ".." then
        Zipper.backward haystack
            |> Result.fromMaybe "Error: Cannot go back any further"

    else
        let
            isNeedleInHaystack list =
                list
                    |> List.any
                        (\child ->
                            let
                                data =
                                    Tree.label child
                            in
                            data.label == needle
                        )
        in
        haystack
            |> Zipper.children
            |> (\children ->
                    if isNeedleInHaystack children then
                        Zipper.findNext
                            (\x ->
                                x.label == needle
                            )
                            haystack
                            |> Result.fromMaybe "THIS ERROR SHOULD BE IMPOSSIBLE"

                    else
                        Err "Directory not found"
               )


addFiles : List File -> Zipper.Zipper Directory -> Zipper.Zipper Directory
addFiles files directory =
    let
        data =
            Zipper.label directory

        exisitingFiles =
            data.files

        usedLabels =
            List.map .label exisitingFiles

        filesToAppend =
            List.filter (\l -> not (List.member l.label usedLabels)) files

        appendableFiles =
            List.append exisitingFiles filesToAppend
    in
    Zipper.replaceLabel
        { data | files = appendableFiles }
        directory


addFile : File -> Zipper.Zipper Directory -> Zipper.Zipper Directory
addFile file directory =
    addFiles [ file ] directory


listDir : Zipper.Zipper Directory -> List String
listDir directory =
    let
        data =
            Zipper.label directory

        directories =
            directory
                |> Zipper.children
                |> List.map Tree.label
                |> List.map (\dir -> dir.label ++ " (DIR)")

        files =
            data.files
                |> List.map (\file -> file.label ++ " " ++ String.fromInt file.size ++ " (File)")
    in
    List.append directories files


printRoute : Zipper.Zipper Directory -> String
printRoute directory =
    -- "TODO: Get route"
    directory
        |> Zipper.label
        |> .label


getLabelFromZipper : Zipper.Zipper Directory -> String
getLabelFromZipper dir =
    let
        data =
            Zipper.label dir
    in
    data.label


getLabelsRecursively : Zipper.Zipper Directory -> List String -> String
getLabelsRecursively dir list =
    let
        label =
            getLabelFromZipper dir
    in
    case Zipper.backward dir of
        Nothing ->
            (label ++ "/")
                :: list
                |> String.concat

        Just directory ->
            getLabelsRecursively directory ((label ++ "/") :: list)
