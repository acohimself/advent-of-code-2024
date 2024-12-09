{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2024/day/8

-}


module Main exposing (main)

import Browser
import Char
import Dict exposing (Dict, empty, fromList, insert)
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (drop, filterMap, foldl, foldr, head, indexedMap, length, map, map2, member, sum, tail)
import Maybe exposing (Maybe, withDefault)
import Set
import String exposing (concat, fromInt, lines, split, toInt, toList, words)
import Tuple exposing (first, second)


title : String
title =
    "Day 8: Resonant Collinearity"


type alias Model =
    { input : String
    , part1Value : Int
    , part2Value : Int
    }


initialModel : Model
initialModel =
    { input = ""
    , part1Value = 0
    , part2Value = 0
    }


type Msg
    = InputText String
    | Solve
    | LoadTestData
    | LoadRealData


type alias Point =
    ( Int, Int )


type alias AntennaMap =
    Dict Point (Maybe Char)


parse : String -> AntennaMap
parse s =
    let
        convertChar char =
            if char == '.' then
                Nothing

            else
                Just char
    in
    s
        |> lines
        |> map toList
        |> indexedMap (\y row -> indexedMap (\x c -> ( ( x, y ), convertChar c )) row)
        |> List.concat
        |> fromList


getAntennas : AntennaMap -> Char -> ( Char, List Point )
getAntennas d c =
    ( c
    , Dict.foldl
        (\k v a ->
            if v == Just c then
                k :: a

            else
                a
        )
        []
        d
    )


part1 : String -> Int
part1 input =
    let
        parsed =
            parse input

        dimension =
            length (lines input)

        freqs =
            parsed
                |> Dict.values
                |> filterMap (\x -> x)
                |> Set.fromList
                |> Set.toList

        findAntinodesFromPair : AntennaMap -> Char -> Point -> Point -> List Point
        findAntinodesFromPair m c ( x1, y1 ) ( x2, y2 ) =
            [ ( x1 + x1 - x2, y1 + y1 - y2 ), ( x2 + x2 - x1, y2 + y2 - y1 ) ]

        findAntinodes : AntennaMap -> Char -> List Point -> List Point
        findAntinodes m c l =
            case l of
                a :: ans ->
                    foldl (\p acc -> findAntinodesFromPair m c a p ++ acc) [] ans ++ findAntinodes m c ans

                _ ->
                    []

        filterInvalid : Int -> List Point -> List Point
        filterInvalid dim ps =
            ps
                |> Set.fromList
                |> Set.toList
                |> List.filter (isValid dimension)
    in
    freqs
        |> map (getAntennas parsed)
        |> map (\( x, l ) -> findAntinodes parsed x l)
        |> List.concat
        |> filterInvalid dimension
        |> length


isValid dim ( x, y ) =
    x >= 0 && y >= 0 && x < dim && y < dim


part2 : String -> Int
part2 input =
    let
        parsed =
            parse input

        dimension =
            length (lines input)

        freqs =
            parsed
                |> Dict.values
                |> filterMap (\x -> x)
                |> Set.fromList
                |> Set.toList

        findAntinodes : AntennaMap -> Char -> List Point -> List Point
        findAntinodes m c l =
            case l of
                a :: ans ->
                    foldl (\p acc -> findAntinodesFromPair m c a p ++ acc) [] ans ++ findAntinodes m c ans

                _ ->
                    []

        findAntinodesFromPair : AntennaMap -> Char -> Point -> Point -> List Point
        findAntinodesFromPair m c ( x1, y1 ) ( x2, y2 ) =
            let
                diff =
                    ( x1 - x2, y1 - y2 )

                negDiff =
                    ( x2 - x1, y2 - y1 )

                findInDirection : Point -> Point -> List Point
                findInDirection ( x, y ) ( dx, dy ) =
                    let
                        next =
                            ( x + dx, y + dy )
                    in
                    if isValid dimension next then
                        next :: findInDirection next ( dx, dy )

                    else
                        []
            in
            ( x1, y1 ) :: findInDirection ( x1, y1 ) diff ++ findInDirection ( x1, y1 ) negDiff
    in
    freqs
        |> map (getAntennas parsed)
        |> map (\( x, l ) -> findAntinodes parsed x l)
        |> List.concat
        |> Set.fromList
        |> Set.toList
        |> length


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputText text ->
            { model | input = text }

        Solve ->
            { model
                | part1Value = part1 model.input
                , part2Value = part2 model.input
            }

        LoadTestData ->
            { model | input = testData }

        LoadRealData ->
            { model | input = realData }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "container" ]
            [ div [ class "w-1/2 mx-auto" ]
                [ h2 [ class "text-xl" ] [ text title ]
                ]
            , div [ class "flex space-x-8 justify-center" ]
                [ a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick LoadTestData
                    ]
                    [ text "Load small test data" ]
                , a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick LoadRealData
                    ]
                    [ text "Load the real data" ]
                , a
                    [ class "inline-block px-7 py-3 bg-blue-600 text-white font-medium text-sm leading-snug uppercase rounded shadow-md hover:bg-blue-700 hover:shadow-lg focus:bg-blue-700 focus:shadow-lg focus:outline-none focus:ring-0 active:bg-blue-800 active:shadow-lg transition duration-150 ease-in-out"
                    , onClick Solve
                    ]
                    [ text "Find solutions" ]
                ]
            , form [ action "#" ]
                [ div [ class "mdl-textfield mdl-js-textfield", style "padding" "16px" ]
                    [ textarea
                        [ class "form-control block w-full px-3 py-1.5 text-base font-normal text-gray-700 bg-white bg-clip-padding border border-solid border-gray-300 rounded transition ease-in-out m-0 focus:text-gray-700 focus:bg-white focus:border-blue-600 focus:outline-none"
                        , rows 3
                        , placeholder "Paste input text here"
                        , required True
                        , onInput InputText
                        ]
                        [ text model.input ]
                    ]
                , div [ class "flex space-x-8 justify-center" ]
                    [ div [ class "textarea_label" ] [ text "Part1: " ]
                    , text <| String.fromInt model.part1Value
                    , div [ class "textarea_label" ] [ text "Part2: " ]
                    , text <| String.fromInt model.part2Value
                    ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


testData : String
testData =
    """............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"""


realData : String
realData =
    """...........6.b....................................
........6................8........................
..Y.......................................o.......
....V...j............B.............c..............
............8.........X.......L...................
.....j..v6.......3.L..................c...........
..Mj.....p3.......b........Z....................J.
..........M...X...................................
V..............v......p.........Z.........c.......
..............3...................................
.......V......U3.............c....................
..........b..v.M.U8...............................
..........j........8.....................J........
..........Y......q........LH..Z...D...........y...
..2Y........PX......6..................BQ.........
...0.Y...............XP...........w...............
.........U.......2...............oH.y.............
0..............9........U.........................
...........P..............W.......z...Oy..........
...................t...p.W..o.............Q.......
.....S.................t.....Q....B...............
S.k..................V..W...p.......H...O......m..
....S.h................W.......................O..
..h..P.2.............Z.............J..............
.........k.......5v.......q...t.s.................
.....Q.....h..........................J...B.......
........0.........l...............................
.S................................................
.............................M....................
2..................e.....o.....y..................
................k.................................
......4......k....t...s.q.........................
.4.......................q........................
.......................z....E.....................
.............0.....d..............................
7..........D........z.............................
.......D..5......7..9.............................
......5..................E........................
D..............K......d..9E..........w.....1..C...
.......K..x.........d....s...........l............
........7......................u...C..............
..K........x..............9..C...u................
4..............s.........................l...T..w.
.......5.....7..................m......T......1...
...........................E...z.m................
......................................u...C.......
.............................em...................
..............................................T...
....................x.......................e.....
.............................1e....w....l........."""
