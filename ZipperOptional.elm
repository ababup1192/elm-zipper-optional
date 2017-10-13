module Main exposing (..)

import Html as Html
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper as Zipper exposing (Zipper)
import Monocle.Optional as Optional exposing (Optional)


type alias NodeData =
    { id : List Int, data : String }


treeOfZipper : Zipper a -> Tree a
treeOfZipper ( tree, _ ) =
    tree


tree2Zipper : Tree a -> Zipper a
tree2Zipper tree =
    ( tree, [] )


(&>) : Maybe a -> (a -> Maybe b) -> Maybe b
(&>) =
    flip Maybe.andThen



-- id(List index)がある箇所のZipperを特定する。


goToNodeById : List Int -> Maybe (Zipper a) -> Maybe (Zipper a)
goToNodeById id mZipper =
    List.foldl (\idx mz -> mz &> Zipper.goToChild idx) mZipper id



-- idで指定した箇所に対するOptional


nodeOfTreeById : List Int -> Optional (Tree a) a
nodeOfTreeById id =
    let
        -- treeをZipper化して、指定されたidまで潜る
        targetZipper tree =
            Just (tree2Zipper tree) |> goToNodeById id

        -- 指定されたidをdataで書き換え、Root Zipperに戻したもの
        replacedZipper tree data =
            targetZipper tree &> (Zipper.replaceDatum data) &> Zipper.goToRoot

        -- 指定されたidのnodeDataを得るgetter
        get tree =
            Maybe.map Zipper.datum (targetZipper tree)

        -- 指定されたidのnodeDateを書き換えるsetter, もし該当箇所が無ければ元のtreeを返す
        set data tree =
            Maybe.withDefault tree (Maybe.map treeOfZipper <| replacedZipper tree data)
    in
        Optional get set


nodeTree : Tree NodeData
nodeTree =
    Tree { id = [], data = "root" }
        [ Tree { id = [ 0 ], data = "aaa" } []
        , Tree { id = [ 1 ], data = "bbb" } []
        , Tree { id = [ 2 ], data = "ccc" } []
        ]


main =
    let
        node =
            nodeTree |> (nodeOfTreeById [ 1 ]).getOption

        -- modify(update)は、チェーン出来る(毎回rootに戻るので微妙に計算量掛かるけど、まあ要素数倍・・・)。
        newTree =
            nodeTree
                |> Optional.modify (nodeOfTreeById [ 1 ]) (\nd -> { nd | data = "new bbb" })
                |> Optional.modify (nodeOfTreeById [ 2 ]) (\nd -> { nd | data = "new ccc" })
    in
        Html.text <| (toString node) ++ "<>" ++ (toString newTree)
