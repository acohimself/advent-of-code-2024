{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2024/day/4

-}


module Main exposing (main)

import Browser
import Char
import Dict exposing (Dict)
import Grid exposing (Grid, fromList, get, height, set, width)
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (foldl, foldr, map, range, sum)
import Maybe exposing (Maybe, withDefault)
import String exposing (lines, toList)
import Tuple exposing (second)


title : String
title =
    "Day 4: Ceres Search"


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
    | LoadFromCache


parse : String -> Maybe (Grid Char)
parse s =
    s
        |> lines
        |> map toList
        |> fromList


listOfPositions : Grid a -> List ( Int, Int )
listOfPositions g =
    map (\y -> map (\x -> ( x, y )) (range 0 (width g))) (range 0 (height g))
        |> foldr (++) []


countMas : ( Int, Int ) -> Grid Char -> Int
countMas ( x, y ) g =
    let
        left =
            ( get ( x + 1, y ) g, get ( x + 2, y ) g, get ( x + 3, y ) g )

        right =
            ( get ( x - 1, y ) g, get ( x - 2, y ) g, get ( x - 3, y ) g )

        up =
            ( get ( x, y - 1 ) g, get ( x, y - 2 ) g, get ( x, y - 3 ) g )

        down =
            ( get ( x, y + 1 ) g, get ( x, y + 2 ) g, get ( x, y + 3 ) g )

        ld =
            ( get ( x + 1, y + 1 ) g, get ( x + 2, y + 2 ) g, get ( x + 3, y + 3 ) g )

        lu =
            ( get ( x + 1, y - 1 ) g, get ( x + 2, y - 2 ) g, get ( x + 3, y - 3 ) g )

        rd =
            ( get ( x - 1, y + 1 ) g, get ( x - 2, y + 2 ) g, get ( x - 3, y + 3 ) g )

        ru =
            ( get ( x - 1, y - 1 ) g, get ( x - 2, y - 2 ) g, get ( x - 3, y - 3 ) g )

        evaluate ( a, b, c ) =
            case ( a, b, c ) of
                ( Just 'M', Just 'A', Just 'S' ) ->
                    1

                _ ->
                    0
    in
    evaluate left
        + evaluate right
        + evaluate up
        + evaluate down
        + evaluate ld
        + evaluate lu
        + evaluate rd
        + evaluate ru


countCrosses : ( Int, Int ) -> Grid Char -> Int
countCrosses ( x, y ) g =
    let
        nw =
            get ( x - 1, y - 1 ) g

        sw =
            get ( x - 1, y + 1 ) g

        ne =
            get ( x + 1, y - 1 ) g

        se =
            get ( x + 1, y + 1 ) g
    in
    case ( ( nw, ne ), ( se, sw ) ) of
        ( ( Just 'S', Just 'S' ), ( Just 'M', Just 'M' ) ) ->
            1

        ( ( Just 'M', Just 'S' ), ( Just 'S', Just 'M' ) ) ->
            1

        ( ( Just 'M', Just 'M' ), ( Just 'S', Just 'S' ) ) ->
            1

        ( ( Just 'S', Just 'M' ), ( Just 'M', Just 'S' ) ) ->
            1

        _ ->
            0


part1 : String -> Int
part1 input =
    let
        countXmas : ( Int, Int ) -> ( Grid Char, Int ) -> ( Grid Char, Int )
        countXmas ( x, y ) ( g, a ) =
            case get ( x, y ) g of
                Just 'X' ->
                    ( g, a + countMas ( x, y ) g )

                _ ->
                    ( g, a )
    in
    case parse input of
        Nothing ->
            0

        Just g ->
            let
                pos =
                    listOfPositions g
            in
            foldl countXmas ( g, 0 ) pos
                |> second


part2 : String -> Int
part2 input =
    let
        countCrossMas : ( Int, Int ) -> ( Grid Char, Int ) -> ( Grid Char, Int )
        countCrossMas ( x, y ) ( g, a ) =
            case get ( x, y ) g of
                Just 'A' ->
                    ( g, a + countCrosses ( x, y ) g )

                _ ->
                    ( g, a )
    in
    case parse input of
        Nothing ->
            0

        Just g ->
            let
                pos =
                    listOfPositions g
            in
            foldl countCrossMas ( g, 0 ) pos
                |> second


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

        LoadFromCache ->
            { model | input = inputString }


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
                    , onClick LoadFromCache
                    ]
                    [ text "Load data" ]
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


inputTest =
    """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX"""


inputString : String
inputString =
    """MMASXSSXSAMXSMMMMSMMXXSXMASAMXSMMSAMXSSSMMMXAAXMASMSMSMXSSSSMSMSMMSMSMASMSAMXXSMMXAMXXXXMMSSSXMSSMSSXMASMMSSSMSMXMXMAXMAMXXXXSXSSMMSSMXASMXM
XXAMXAXAXXMAMAASXMXSAXXAXXMASAMXMMAMAMXAAAAASMXSMAAAAAXSMAMXAAXAAXXAAAXMXSMAXMAMXASMMSMSAAAXMAMMAAASMXMXAMAMAAAMASAMXMASMSSMMMAMAASAMXSSSMMS
MMXSAMMMMSMASXMSAAAMMMSMMXMAMASMMSSMMSSSMMSMXAASMMAMMMMXMAMMSMSSSMMSMSMSAMXXSMASMAMSAAASMMMXXAMSMMMMXAMMMMASMSMSASASXMAMAAAAMMSMSMMAMXMASAAA
XMAMAXMAAXXASAAXMMMSMAXMAXMASXMAXAXAXMAMXAXMMMMMAXMXMSASAASAAAXMAXMASAAMMSSMMMAMMAMAMMMMSMMASMMSAMXXSXSAXXMSAAAMAXAMXSMMMMSXSAMAAXMAMMMXMMMS
MMSXSMSMSSMSSMMMSSMAMASASXMAXAMSMASXMMMMMMAXMXMSXMXAMXAAAMMXSXMSXMSMSMSMAAMAAMAXMXSMMMXAASMAXMASMSMXMASXSMASMMSMXMAMASAXSXAAMASMMMMMXAMAXXAX
MAXXMAMSAXMASAAAAASXSASAMAMSXSXAAMXMXSAAASMSMAMMMSSMSMXMMMXAMAXMMASASAMMMMMSMXMMMXXXMMMSMSMSSMMMAXMASMMAMMAMAXMASMXMASAMXMMMMAMXXMASXSSMSMSS
MAMAMMMMMSMASMMMXXAXMXMAXSXXAXMASMAMMAMSXSAAXAXAAXAMAMXXSXMASMMAXXMMMAMXAMXMSAAASMMMSXMAMXAAMMXMMMMAMXMAMMASXMMASXAMMMXMXXAXMASXSSMSAASAAAAX
MXSMMMXSMXMXSXSAMXMXMASXMMAMMMSXAMMMAAMMMMMMSSSMSSSMSSXXMAAAAXXSMSMXSMMMAXAASMSMSAMAAAXXSMXMXAMXMMMAXASAXXASMMMASMSMAMAXXSSMSASAAAAMMMMSMXMS
SAAXSXAAXAAMMXMASXXASXSXAMAMAAMMMMMXSXMXAAAMAMAXMAXAAMMMSAMMSSMAASMXXMMSASMMMAMASXMMSSMAMMMXMXSAAASMSMSASMASAAMXXXXSASXSXSMXMASMXMMMXAMXSXAM
MSMASMMSSMXAAAMAMMMXMAMXAMXMMXSAMASMXAXSMMMMSSXMMSMMMXAXAXSXAAMMAMXSASAMXAMAMAMAMMSAAXMXXAMAAAXMXMMAAMMAMMMMMSXXXMAMMSAAMMSAMMMXXSXSSXSASMAM
MASMXAAMAMXSMMSMSASAMMMMMSMMSMSMSMSAMSMSASMXMXMSAMAXASXSMMSMSSMMXSAXXMXAMMXAMMASMAMMMSMASXSMSSXSXSMSMMMMSSSMXXASMMSSMMMSAXSXSAXXMMAXAAMASAAX
MAMXXMMSAMMXAXAASMSMSAMXAAXMAAMAMMMMMAAMAMMASAAMASMMMMXAXASAXXXXMMXSAMSSSSSSSMAMMXMAMAMASAAXAAMSAMAMXXXXMASMAMXAAXAAXAXMMXSASMSMMMAMSMMAMXMS
MSSSMSASASASMMSXSXXXSASMSSSSMMMAMAAAMMMMMMSAMAMMMMXAMXSXMASXMMXMXAAMAMAMAAAAXMASMMSASXSXSMMMMXAMAMAMXMMXMMMXXMMSMMSAMXSAMXSAMXSAAXXXAAMXMAMX
SXAAAAAXAXMAMMMASXSMMAMXXAXXMXSMSMSSSSMSAAMMSSMSXMAXMXXAMASAXMAMMMXSAMXMAMMMMMXSAXMXMAMMSAMAMSSSMMMXMAAXSMMMASAMXAMXSAMASAMMMXMSMSMSXMMSSMSX
XMMMMMAMMMSSMSMAMAMXMSMMMXMXAMXAAXMAXAASMSMMAXMXAMXSXXSXMASMMSMSAAAXXAXXAXAMASAMXMXSMSMASASAXAASMAAASMMMMAAMMAAMSMSASASAMXAMXXXMSMAAAMAXAAMM
XSXXXXXXSAAMMAMAMAMMXAASMSMSAXMMMSMSMMMXMMAMXMSSSMAMMMMMMMSAXMASMMSMSXXSSSXSASAMXXMMAAMMSXMXMXSMSMSXSAXASMXXMMXMAAMASAMAXXXMXMMSASXSMMXSMMMA
XMXMASMMMSSMSXSAMXMAMXMXAAAMXMMAXMAMXXMASXMMSMAAAMXXAAAMAAXXMAMMSSMMSAMXXMASXXAMXXAMMMMAXAMASXMAAXMASXMMXXAMMSSMMSMMMMMMSSMMASMSAMXMXSAMXAXX
AMAAAAASAMXAMXXXXAXSAMXMMMSMAASMMMMMMXSAMXAAMMMMMAASMSXSAXSAMXXAXSAAMAMASMMMMSMMMSSSMSMMSMMAAAMSMSMAMMSAXSSXAAXXAXXXAXMAAAASAMXMMMXSAMASMMSS
MSASMSMMAMMXMMMSSSSMMAAAMAXXMMAXXAXSAAMXSSMMSAMXMMXSAAXXAMSAMAMMMXMMXXAAAXAAMAAAXAMAAAAAAXXASXMAMAMAMSAMXAXMASMMSSXSAXMMSSMMASMMXSAXXMSMMAAA
XAAXAXXSMMSMSXAAAXAASXSSMXSXXSMMMMSMMSMXAAAASXMAXXAMMMMMMMXAMMXAAXSSMSMMMSSSSSSMSMSMMMMSXMMAMXSXSSSSMSAMXMXAMXAAAAAXMASXMAAMAMAAAMXMMMMAMMMS
AMAMAMXSXAAAAMSSXSAMXAAXMAXMXSAAXSXMAMXMMSMMSASMSSXSXAAAASMMMXSMXXAAXSASAMXAXAMMMXXXSXAMMSSMSAMXXMAXXXXAAXMSASMMMMSMMAAASMMMAXMMXSASASMAMXXA
XSAMXMAXMXSSXMAMMMXAMXMAMSXSASMMMXAMXXAXXAMAXXAXXXAXXMSSMSAMAXMASMSMMXAMASMSMXSAXMAMXMAXAAAXSAMMSMMMMMMSXSAMMXXMMMMAMASAMXSXSMXSMSAXSAMXMMSS
AMXSAMXAMXMAMMAMMMMXMASMMAAMASXXMSXMAXXMXXMXSAMMMMMMXXAXASAMXSMMMMAAAMSMXXAXMASXSMASAXMSMSSMSAMAMAAAAAAXAXXXAXXMAXSAMXMAXAXXMAXMXMXMXMMASXAA
XXAXXMSXSAMAMMASMXSASASAMMSMAMMAMMMMMMSAMXSASAAASAXSXMASMMXMASAAXXXMXMAMSMSMMXSAXMASXAXAAMXASAMASXSMXXSMXMASMMMSAXXXSAMMMXSAMASMMMMMMXSASMMA
SMXMXXXAXXXAMXAMMASXSXMAMAXMAMMAMXXSAAMSAXMASXXASMXSXMAMXSSMMXMMMMSMSAMAAAMASMMXMMXXMXMMXMMXMXMMSXMMSSMMAAAMAAAMMMXSMMXMASMAMASMAAAMSAMXSAXS
SASMSMMXMMSSSMSSMMMAMMMSMMSMSMSMSMAMMSMMMMMMMAMMMMAXAMXMMAAAAMSSMAAAMMXSMSMAXAASXXASXSXSAXSMSSMXMASXMAAAMMSSMMMSSMMXAAAMSMSXMASMSXSXMMSMMMMX
MAMAAAMASXAAXAXAXXMMMAAXAXMXXXAAAMXMAMXMAXSXMXMSAMASMMSSMSXMASAAMSMSMXMXXAMXSMMSAMXXAAAXMSAAAXXAAMXMMSMMSAMMASXXAMMSSMXSXASMMMSXXXXASASAAXAS
MAMSMXSASMMSMSMMSSMASMSSMMSAMMMSMSMMASAMSSSMASASAMXSAAAXXMAASMXSMMXXMASXSAMXMXMXMMMMMMMMXAMMMSSMMMAMAXMAMASMASASAMXMXAXSMMMAAMXAMXSAMASMMMXX
SSXMMMMMSAAMMMAXAASAXSXAAAMASMMXXAXMASMSXAXMAMXXSMMXMMMMXXSXMAAMMSAXSAMMAAMXAXMAMAAAXAASXMXMAXAAASASASASXMMMASAMMSXMMSMMAASXMSMSMXMAMAMASXSM
SXAAXXAAMMMMSMMMSMMMSASXMMMAMXASXMSMASXAMMMMSSMMASXSMMAXMXXASMXMAMXXMASMSMMMMMMAXSXSSSSSXXAMASMSAXAMASAMXSAMXSAMXSAXMAASMMMMAXAMXAXMAMXXMAXM
MSSMMSMSSMSAMXMXMMAXSAXXAMXSMMAMAMAMMSAMXXAXAAASMAXAASAXXASXMASMAXMASAAXAMMAMMMSMMAMAMAMMSMSAXXMXMXMXMMSXSASAXMMXSAMSSXMMMSMSMMMXMSAMXSMMMMS
XAMAXAXXAAMXSAMXAMAMMAMSXMAXAMXMAXAMXSMXMXSSSSMMXMMSMMASMASAMAMSMMXMMMXSASXSXSMMAMMMAMAMXAMXMXXSMMSSMAAMMMXMAXMMMMAMAXMASAASAMXSAMMAMXAAAAAS
MMMSSMSSMMMASMSSXMAAMMMSAMASXMMMAXXSMXMAMSMAAXAAAXAAXMAMMXMAMMXAMXMXAMAXMAMXXSAMMMASMSSSMMSMSXXXAAAAMMMSSMSSMMSAASXMXSAAMMXSMXAMMSSSMSSSMMSS
MAAXAMXAXSMMXXMASMXXXAAMXMASMSAMMMSAMSMMXAMMMMMSXASASMMMSSSXMXSXXSAAXMSSMSXSASAMXSMSAAMAASAASMXSMMSSMXAAAXAAAAMXMSXAAMMSSSXMXMMSXMAAAAXMMMXX
MMMSAMMXMMASXASAMSAXXMSMXMASASMSAXXAMSASMMSAXAAXXMXXXAXAAAAXSMAMXSSSSXXAAXAMMMASAMXMMSMSMMMAMMXMMAMAMMMSSMSSMSSSMXMMMSMAAMAMMMAAAMSMMMSMMSXM
SAMMXMMAMXAMSMMAMMMSAAAMSSMMAMAMMSSMMSXMAXXASMSMMMASMSMMSMMSMAAMXXAMXMSMMMMMASAMXMXAXXAMXSMXASMMSASAMAXMMAXMAAXAXXMMAMXMAMAMAMSSSMMXSMAAASAM
MMSMAASASMAXXXMXMAAXMMMSMAMMAMAMXMAMXXMSXMXMXXAAASMXAMAAAASXMMSXMMSMXMASAAAMXSSXAMXSAMAMAXMSXAAASXSAXXXMMSMMMMSAMMMSSSXXASASXXXMAMXASXXMMSAM
MAAMMMMAXXMMXMXXMMXSAXMAMAMSMSSSXMAMMAMXMMASMMSSMSAMSMMMSXSASAMAMXAMAXASMSSSXMMMXMAMXSAMXSAMXMMXSXMMSSXMAXXSAMXAMMAAXAXXXSAXXMASAMMXMASMXSXM
MSSSSSMMMSXMASASMAXSAMSMSXMXAXMAMMSXSAMAMSASAAMAMMMMAAAXXMSAMASMMSSSSMAMAXMAMMAXAMMSAMASASASXXSASMSAAXAMXMMSAMMAXMAMXAXMMMXMASMMSMSAMXMMAMXX
XXAAMAAXMMASXAAMXAMMMMMXMMMMSMXSXMAASXMAXMASMMSXMAMXXXMMAMXXMAMXAXAAMXSSMMMAAXMSXSAAAMAAASAMMMMASAMMSSMMSSMSAMXXSXSMSSSMAAASXMXAXXMXSSXMAMMM
MSMMMSMMSSMMSMSMSMMMAXXAXAAAXMAXAMMMMMMSXMXMXMXXMMSSMSSXMMMAMXMMSMMMMAXAAASAXAMSXMMSMMXSXMXMAAMAMMMAAAAAAAXSMXSXMAMAMAAMMSMXAMMSSMMSAMASAAAM
AAAAXAXAAAAAXXXAXAASMSSMSSMSAMMSMMSAMAAAMSSMMAMMMXAASAMMSAMXAAXXAMXMMMSMMMSAXSXSAMAAMXMXXXXMSASASAMMMSMMSMMXMASAMMMAMSMMMXMMAMAMAMSMSAASMSMS
SSSMSMMMSSMMSMMMMSMMAXMMAMXMMXAAAAMAMMMSXAAAMMSAAMSSMMSASMSSMMSSMSAAAMAMAXXMXMASAMXMMMSAMXMAAAMASAMSMMXMAXXSMMSAMAXAXAXXMAMXAMSMAMXAMMASAMAX
XMAAAXMAXMAAAMMAMXAMSMSSSSSSMMSSMMSMMSAXMMSMMXSASXMAAXMMSXXAASMXXSXSMMASMSSMAMAMMXSXAXMAMXMMMSMAMAMSAMMSAMXSAXMMMSXXSASASAMSAMASMSMAMMXXAMXM
SSMXAMXMSMSSXMSMMMSMMAXAAMAXSAXAAXSAAAAXSMMAAXSAMXMSSMSAMXMSMMAMXMAMMSMMMAAMAXAXXXXXAASAMXXXAXXAMXASAMAAAXASMMMSMMAAXMXMMAMSAMXSSMMSXXMSSMSA
MAASMXMXAAXXMXAAMAXAMSMMMMMMMSSMMAMMMMMXMASXMAMAXXMMXAMXMAMAMXXMAMMSAXAAMSSMMSMXMMMSXMAAMMMMMSXMXXMXXMSSMMMXAMAAAMXMXMSAMXMSMSAMXXAXXAAMXAMS
SAMAMAXSMSMASXSMMASXMXAASAAMSAXMAASAXXMASMMASXSSMMMAMXMASMSMSMASXSMMMSMMMAAAMAXAAAASAMSXXAAASXMSMXMMXAMAMXMSAMXMSMAXAMXMAMMXAMXSXMASMMSSMMMM
MAMMSAXXMAXAAAMAMAXAAXSMSMXXMASXSMSMSSSXSXSAMXMAAXMAMXSXXMAAAXXMAXAAMMMXMMMXSAMMMMMXAMAASMSXXMAAMASMMSSSMAMMXSAXAXXMMSAXMASMMMASXMXAXAAMAMXA
SAMXXMAMSAMMSSMSSSSMMMMMXMSAMMMMMAXAAAXASAMXSAMSSMSSMMMMMSMSMSMSSSSMSAAXXXAMMMSSSSMXSMMMMMAASASAMXSXAMAMMMSAAXMSXXMAMMXSXMXAAMASAASXMXMSAMXM
SSSXMASAMXSAAMAXAAAXAAXAAMSXMXAXMAMMMSMAMXMAMAMAMAAMMSXAMAAMAAAAAAAMXMMMMMASAMSAAMAAMMSXAXMAMMXSMASMSMAMMAXMASASAMSSMMSXMASXMSASAMMAMXMMAXXM
MXXXMAMAMXMMXSMMMMMSSSSMSXSAMSMSMSSXXMMXXMASXXMASXXMASMMSASMSMSMMSMXAMAXSMMMASMMMMMMSAMSAMMAXXSAMXMAXMXMXSXMAAAMMMAAAXSAMXXXAMASAXSAMAMSSSMA
XSMSMXSAMMMXXAAAXAAXMMAAMAXAMMAAAXXXAXMMMMAMAMSAMXMMASAXAAMAXXXMAXXXXSAMMAASXMAAXXAAMXMASXSMSXMAXSSSSMSMAMAMAMSMXMAMXMMAMSMMMMMXAMXAMXMAAAXS
MAAAAXSAMXAAXXMMSMMSMSMMMSSSMMSMSMXSAMXAAXXMAMMSMASMAMXMMMMXMSSMSSMMMMXXSAMMMSAMXSMSSXMXMXAAMMASAMAASAAMAXXMSXMASMASXMMAMXAAAAXSSXSSMSMMSXMA
SMSMXMXMMMAMSAXXMAXAAAXXXMAXSAMXMAXMASXXSXMMSSMASASXXSAXXXXAMXXAMAXSAMMMMXSAAXAXASAMXMMASMMAMXAMAMMMMSMXMSAMMAMAMXXMAASMSXSSSSXMMAAXAAXXMMSM
XXXXSSSMXMAASAMSXSSMSMSMMMXMMXMAMXMMXMAMAMSAAAXAMXXASAMXXAMXAAMMMXMSASAAAASMSSSMAXSAXXSAMXMAXMXSXMASXXSAAAMASXMMSXMSSMMAMAMMXXAAMMMMMMMAAAAX
MMMAMAAXXSSMMMMMAXSMXMSAAAASMSMMSASXSMSMMAMASMMXXAMXMMXAXSXMAMSAMXASAMXMMXSAXAXAMXMMSMMMMAMSXSMXXMAXMAMMSMSAXXAMXMXAXAMMMAMXXSMMMAMAAAXMMSSS
AAXAMXMMMMAXMXAMSMXMAMXSMXMMASAAAASAMAAMXSMAMAXMASMMSXSMMXAMMMSASAMMSMXMXAMXMAXSXSAAAAASMSSMASXMSMSXMXMAXXMMSSMMMSMMXSMXXASMMMAXXAXSMSSMAMAX
SMSASXAAMSAMXSXXXASXSSMXMXSMMMMMSAMAMSMSAAAAXXMXAXAMXAXXASAMXASMSXAAAMASAMSXSSSMXXMMXSMSAMAMXMAMSAMXSAMMXAXAAXAAXMXSAAMAXSXMASMMSSXXAAAMAXMX
MAMAMXSMXXMSMMMAMMXAAMAMSAMAMXXXMXSXMMSMMSSSSMMMMSAMMAMXMAXXMASASXMSMXAMMXXAMXAMAMSSXAMMASMMMSXMMMMASASASXMMSSSMSXAMMMMMSXAMXSXAXXAMMMMSSSMM
MAMSMMMSAMXAAAMAXMMMMMAMMAXXMAMXAMSASXXMAAAMAASMMSAMMSMMMSXXAAMXMMMAXMXXXXMAMSSMASAASXMSXMXAASAMXAMXSMMMSXAMXAAASMMSXXSXMXSMAMMMMMMMMMAAAAAA
MXMXAAXXMASMSXSASMAXAMXXSAMMXSAMMMMAMMAMMMSMSXMAAXAMAAASAMSSMXMMMAMAMXMASMMAXAMXMMAXMSAMXMXXXMASXXMASMSXSXSMMMMMMAXXXSMAMMMMASASASAAAMMSSMMS
SSSSSMSMMMSXMMMAMMASMSMMMXSAAMXMAMMXMXAMAAXAMXSMMMMMSSSMAAAMMSAMMSMMSSXMMASAMMSXSSSMMXMSMSMSXSAMXXMXSASAMAXMXMAMMSMXXMMXSAMXXSASMXXSXMAAXAXX
MAAAMASASAMXSMMAMXXSAAAXMAXMXMASASMMSSSSMSMSXAMASAMXXXXMMMMSAXXXXAMSAAASXMXAMXAAXAMSXAXAAAAMAMASASMMMXMAMMMMAMASMAMSASAMMAMMXMMMMMXMMMMSXSMS
MMMMMXMMMASAMXSSSSSMXSMMMMSSMXMMMXXAMAXXAMAXMXSAMASMAMMXXXAMXSMMSASMMSMSAMSSMMMMMAMXSXSMSMXMASMSAMAAAAXXMXAMXXAXXAXXAMMXSSMMMMMAMXAAMXSAAXAM
XMAXSXXXSAMMSAMAAXMXAXXXAAAXMASAMXMMMMMSMMXMAMMMSMMXSMSASAMSSXAASXMXXMXSAMAMXXSXSXMAMMAMXMASXSAMASXMSSSMSSSSSMSSMMSMMMSMAXXAAASASXMSMXMMSMAM
SSXXAXSAMXSAMAMMMMMSXMXSXMSXSAMASMSSMSASASXMAMAMXAMSMAXASMXSMMMMSAMMMXAMAMAXXAMMMAMXMAAMXSASASXSAMAAAAAXMAMAMXAXAAAAAAAMMMMMXMSMXMXAMXXXAXAM
MASAMAMASAMXSAMAXMAAMSMMMXAAMAMAXXAAXMASAMAXASAMSAMXMSMMMXMMAMMASXMASMSMSSSSSMSASMMSASXSAMMMAMAMXSMMMSMSMAMMMMMMMSSSMSXXSAMSSMMMSSSMSMSSMSSS
MAMMXMSAMMSASASXMMMXXAAXAMMSMSAMXMMMMSAMAMSMMSAXXXMASAMMMXAMAMXMMASXXAXAMXAAXAAMXAAAMMMMASAMMMSMAAAAXAXMMSSXAAXSXAXMAMASMSMAAXAAAXAAMAMMMAMA
MAMXSXMASAMXSAMASXAXMSSSXSAXAMAXAAAMMMASXMAAAXMAMSSMXAXSXMASMSSXSAMXMMMMMMMMMMMSSMMMXMXSASASXAAMASXMSSMSAXXXMMMAMASAMXMMAMMXMMMMMSMMMAMAMASM
SXSAMASAMXMASMSAMMASAAAXAMXMASXMXSXSXMAMASMSMAMSAMXMMSMSASXSAAAMMAMSMXAAAAAAASAXAAMXXSXMASAMMSMSAXXAMMAMXSMXSAXMSXMXMXXMAMMASXXXAAAASASMSASX
AAMXSAMXSXMASXMASXAMMMMMSMXSXXAMXMMMMMAMXMMMMXAAXXAMXXAMAMAMMSMMXAMAAASXSXSXSMMMXMMSMMMMMMMMMAXMMSMXSMSMMSAASXSXMMMAMSXSXMXAXAASMSMMMMMXMASA
MMMAMASXSAMXMAMAXMXSAAAXAMMXSMXMXMASAXXMAXAAMXMMSXSAMAAMSMXSAMXSSMSMSMMAMXXMMASXMSASAMAAXAXAXSMAMXMXXAXAAMMMMXMASASMSMMMASMMSMMMAAMAXSSMAMMX
XXMASAMXXAMSMSMSSMSSXSMXASMAMMXXMSMMASXSAMSMSAAAAXMASMMMXAXMASAMAMMXXASMMASASAMAAMASAMSSSMSXMASASAMAMSMMXMSAMXSASMAMAAXMAMMXAXAMMMMMSXAMXXMX
MXSXMASMSSMSAMMXAMAMXMXMSAMXSAXSAMXXSAMMSXAASMSMSSMAMMASMSSSMMMXAMXMAXMMMAMXMXSMMMXMMMAAAASAMXXMMAMAXXASAASXMXAAMMAMMMMMXSXSXSXXSAAXMXMMMSXX
XASAMXXAXAAMAMMSMMASASXXXAXXAASMMMXXMAXAMMMMMXXAAXMAMMAMAAAAASMSSMSAMXAXMMSMSXXMASMAAMMSMMSSMMXSXXMSSMAMMAMAXSMSXSMSMAMXAMAXXMAXSMXAXAXAASXM
MXSMMSSMSMMMAMXAMSMSXSXXMXMXMMMMAMXXSMMASXASAXXMASMAXMXSSMSMMXAAAAMSMSAMXAAXMSASASMSMSAXAMSAXSASAMXMAMXMXXSSMXXAXXAAMAMXAMMSAMSMMAMSSMSMMSAX
SASAAAAAMMMSSSMMMMASAMAMSASASMXSAXMAMSXXMMAMMXSMAXMAXXAMAXMMXMSMMMMAMMAMXSMXXMXMAMXMAMMMAMSMSMASMMAMXMXSMMAMSMAAMMMMXSXMXAASAMAAMAMMAMSXAMXM
MASMMXMXMAMXAAAMASMMMMAASASASXAXMXMAMSSMMXMASAMXMXMMMSASAMXXAAAXAMSMSSSMMAMAAMXMAMAMXMMSSMMXMMAMASMSXXAAXAAMAMMXMXASXMMMSMMSAMSMMSMSAMMMMSMA
MMMXAAXASAXXXSMMAMAAXSMMMMMAMMMXSAMAXMAMXSAXMASAMAAAASAMXSMSMSMSSXMMAAAMSAASAMMSSSMSAAXSASMMMMMSAMXAAMSSMXXXAMSASMASAMXAAAMSXMMAAMAMAMXAXAAM
MAMMSMSASMSXXAMMSSSMMXMASAMAMXMASASXMSAMASASXXMASASMMXSMXAXAXXXMMASMSXXMMMMMAXXAAMAMMSMSAMXAXSMMMSMMSMAAAMSXSXSASMMMAMMSSSMMMSSMMSMMSSSMSMSA
AAXMAXMAMXAMXAMXMAAAXSSMSASXSMAMSAAAMXAMXSAMXMSMXMAXSMASXXMMXSXXSXMAMMMMXAMMSMMMMMAMXMAMXMMSMSAAAMAMXXMMMASAMAMMMAXSXMMAAXXAAAMAMXXSAAAAAAXA
XXXAMMMXMMXSSXSAAMMMMAAAXAMAMAAXMMMXMSMMMMAMAAAMAMAAXSAMMMMSAMXMMMMAMMASMXSAAAXAASASAMXMXXXMASMMMSAMMASXSAMAMXMASXMSAMMSSMAMSMSAMXMMMSMMMXMA
MMMAMXMXMXXAAASMSAAXMSMMMSSMASXSSXXXXAAAAMASXSSSSMAXMMXMXAAMASMMAAMAXSAMAXMMMXMXXSASAMXAMMMMXMXSAMXSXMAAMSSMMMXXAAAMMMAXAMMAXMSASAAAAAAXXAMM
SMASMMMAMXSMMMMXMXXXAMAMAAAMAAXAMXAMSXSSMSASAAMAXSMSMMMSSMMSXMASXMMMXXSMMMXSSSSSMXMXMXMXSASMSMMMXSAMMXMXMASMAMMMAMSMSMSSSMXMXASASAXMSSXMMSSM
AXAMAMMXSAAXXSSSSSXSAMXMMSAMXSMMMMSMSAMAMMXMMSMMMXMAXAAMAMXMASAMXSXXSAXXXMAMAAAAMASMSAXXMAXAAAXAAMAMAXXMXAXMAXXXAMXMAAASXMAXXMSXMAXMXXXAAAAM
XMXSXMAAMXMMMXAAXAXSMMSXMXASAMAMXMAMMAMMSMAMAXAXMMSASMXXAMXXMSAMASAAMAMMSSSMSMSAMXSAXMXMMSMSMSMMSSXMSMSAMSSSSSMMXMXMMSMMSSMMMXSAMXMSAXSMMSSM
MAAXMAMXSAMXSMMMMMMMMASAASAMXSXMASASXMMAAMAMASXMSAMXSMMMSSMAXSAMASMXMAMAXAAAXXXASMMAMSMSAMAAMAASMAMXMAMMAXAMXAAMXXAXMAAAAMAMAASAMAXMXMMXMAAX
ASXMASAASAXXAXSAXMAAMASXMMXMXAXSMSMMXMMXSSSMASMXMASXXAAAAAXAMXMMASAASXSXMSMMMSSXMAMAMAAMASMMSSSMMASXMAMMSMSMMSAMSSXSXSMMMSAMMXSMXXMSAMXASMMM
MMMMAMMMSMMSMXXMSSSMSAMMSMASAMXSASASXSSXXAXMAXAXXXMASMMSSXXMXXMMXSMXSMSMMMAAAXXXMAMMSMSMMXAXXMAXMASAMAMXMAXXAMAXAAXMAXXAMSASMMMXSMMMXMSMSAMS
XAAMASAAXAAXXMMSMAAXMASXAMXMAXAMAMXMAAMMMMMMASXMMMXMASAXXXXMMSXSMSXXMASAAXSMSSMSMAMXMXXMMSMMSMMSMASMMMXAMXXMSSSMMSMMAMMSXSXMXAAASAMMSMSXSAMA
MSMSXSMMXMMMMAMMSMSMMMSMMSMSMMMMMMXMMMMAXXXMAXMAAMAXAASAMXMSAMXSAMMSXAXMMMMMAAAAXASASAXAAXMAMAAAMAMXAMSMMSAAXAMXXAAXAXXXASXMSMMMSAMXAAXASXMM
XMASAMXMMAMAXXMMXMMASAMXXAXAXAAAAXMASXMSSMXMXSSSSSXSXMAMSAAMMSAMXMAXMSSXSAXXSMMMSMMAXMMMXSMSMMMXSASMMXAAASMXMMMMSSSXSXSMMMAXAAAXSMMSASMMMMMM
AMAMXMAXMAMSSSMSAASAMMSMXMXXXXSXSSMMXSXMXMXMAXXAXMAMXXAMSXSSXMASMMSSMXAASMXMASXMAXMXMSASMMXXAAXXSAXAXSAMMSXXSAAXAMXAMMSASMXMMSXXMAXSAMXXAXAX
SMMMXXASMMMXAAASAMMXSXAAAMSXMXMMMAASAMXMAMAMXXMMMSSSSMSMMAXMAMAMXMASMMMMMASMMMMSAMSAMMASASASMMMXMSMAMMASXMMMMSSSSMMMXASAMSAMXMMSSSMSAMAAMSMS
AAAXXMASAMXMSMMMSXAMXMSSMMAASASMXXMMAMMSASASMMSAAXXAXMXAMXMASMMSSXSXSASASMAAXAAMMMSAXSAMXMASAAMSAMXSXSMMAASXXXMAMXAMMMMXMAMMAXAXAMAMAMXXXAAM
SSMSXSAXMMMMASAMAXMSMAXXAXSXMASAASMXAMMXAMASAAMMSSMMMASMMXMAMSXAMXMASMSASXMMMMXSAASAMXAMXMXMMMMMAMAMASAMSMAXMSMSMSMSAMAMXMMSXSSMAMSSSMXMMMSM
XAASAAAXSAMSASXSMMAAMMMXMMMXMMMXMAASMSMMSMSMMMSAXAMXAXAXAAAAXXMASAMXMXMXMXXXXMAXMXSXMSXMXASMMSXXAMAMMSMMMMMMMAXAAXMAMMASMXMMAXASXMASXAXSXAAM
SMMMMMMXMAMMAMMSXMSXSASXSAMAMXMXXMMXMAXAAXMASAMMSMMXMMSXMMSSSMSASMSXSSMSSXXAXMXSMAMAMXAMMXAAAXMMXSAMAMXAXAAMSMSMXMXMSSXSXAXMMMMXXMXSMAMSAXSS
MXMAAXXASXMMAMASXMAASASXMASMMAXMMXSAXMMSSXSAMXMMAMMXMSMSMAXAAAMASXSAMXAAASXMXMXAMMXAMSMMAMXMMSMSMMAXSMMXSMMXAAAMASAMXMMMXMSMAAXMMXXXAMXSAMAX
AAXSXMXMMMXSXMAXAMMXMAMXSXMXSXSASASMSXXAAMMMMMMSAMSAXAAAMXMMMMMXMASMSMMMSMXSAXXXMXSSXXMAMMAXXAMAAMSMXAXMXMXSXXASMSASMMAAXMAMSMXAAMMMSXAXXMXM
SSMXAMSSSXAMXMASMMSAMXMXMAMXSAAXMASAAXAMAMAAMXAMAXXMXMSMMAXXMSXXMASMSAMSMXAMAXSAMXMXASAMAXMSMMSMSMMASAMMASMMSMAMXSAMASMSXSAMXAXSSMAAMMMMXXMX
MMAMXMAAXMXXMXAAAASASXMSSSMMMMMMMXMAMXSMMMSSSMXSMSSSMAMASMSMASXMMASMMAMAMMMMAAXAMSMAXMAMXASAMXAAXXMAMMSMASAAAMAMXMMMXAXXAMMMMSMAAXSSXAMSSMSM
MMAMAMMSMSSMMMSSMMMMMXAAAAXMAXAXMXMXMAXAXAXAMXAAMXAXMAMMXAAMAMAMMASMSMMAXAXMXSMXMAAXSSXMMXSASXMSSXMXXAXMMSXSXSASXMXSASAMXMXSAXASMMMMXSAMMAAX
MXAXMSAMAAXSAAMAMAXSAMMMSMMSMMSSMAMAMSSXMMSAMMMSAMMMXXSAMMMMMMAMAMSMAASMSMSMAXXASMXMXMASAMMAMXMAMMSXMSMSAMAXAXASMSAMAMXMAMXMXSXXXXMAAMAMMSMM
SMXXXMAMMMSAMXSAMMXMAXXAAAMXMAMMMXXXXAXMAMMMSAMAXAXSMMXMASAAXSSMSSSXMXMAAAAMAMSXSAAMSAMXSAMXMXMMSMMXXXAMXMMMSSMSAMAMXSASAXXXASMMSXSMXSXMAMAS
MASMSSSMAMSMSXMXSXMSSMMSSSMXMXSAASMSMAMSAMAAMASAMSMMASAMXMXXXSAAMAXMSMMXMXMMSMMASMXMAAXMASAMSASAAASXAMAMMMXAXXAMXSXMAMMSMSSMMXAAAMMXXMAMASAA
MAMAAAMMSMMXMASAMAXAAAAMMMMMMSMMMSAAMSMSASMXSAMMXXASMMAMSSSXXMMMMAMMAMXMSASAAAMXMMSSSSSMAXMASASXXSMXMSASAXMXXMXMASAMMSAXAAXASMSMMAMSMSAMAMAS
MXSMMMMAXMASXAMASXMSMMMSXMSASAXSMMMMMAASAMAAXXXMSSXSXMAMXAAXXXXXMSMSASMAXAMSSMXXMAXAXAAAAAXXMMMMMAMAXSAMXSAMSMMMMSAMSMMXMAMXMAXXXSXMASASXSAX
AASASMMXSMMMMMSXMMMMXSAMMASXSXXMASAXXMMMAMSMXMMXMXASMSSMMXMMMMSMAMMSASXSMMMMMMMAMXMMMXMMASMMMSAXMAXMMMSMXMMXAAAAXSXMSASXMMSSMSMSMXASMSMMAMAS
MMSAMXSASXMMMMAMXAAMXMAXMASMMMAMAMXSMSASMMAMXXMAMMMMAAXMSSSXXAXAMMASAMAXAAMAAXSXMAMXSAAXAAXAAMXSSSSXSAXMMMSSSSMXASXMSAMXAAAMXMAXASMMASAMXMAM
XAMAMSMAMSMASAAMSMXSASXMMMSAAAMMSSMMAXASAMAMXMMASAMMMMSXAASAMMMMMMASAMXSMMXMSMMSXXSAMSAMXMMXMMXMAAAAMMMMAXXAMXXSAMXXMAXSMMMSAMXMXMXMXMASAMXS
AMSSXXMAMAMASMMMSXMSASMMAMSXMMSAMAAMAMSMXMASAMSASMMAAXXMMSMMAMXAMSMMXMMMMSXMXAAAXMMXXAXAMXSMMSAMMMMMMAASXSMMMAAMMMMXSAMAAAAXMSSSSXMXSXMSXSAM
XMAMAMXASXMASASAXAAMAMAXSXXASAMAMSMMXXXAMMAMAMMXMASXSMSASAMSASMSMAAXSMMAAXAXSMMMSXAXSMMSXAXAASMMMMAMXXMMSAXAMSSMXAMXSXASMMXSAMAAAMSAMXXSMMAS
XAASMSSMSXSXSXMASMMSMMSSMSSMMASAMMASMAMAMSSMSMSXMXMAAAAMAXXMAMAASMMMMASXSSSMMXSAMASMAXAAMMMMMSAAMXSMMMSMSXMMMXAASXSMXAXXXAAMAMMMMMMASAMXASAM
XMMMXMAAMASAMAMAMXAXAAMAAAXXSAMAXSAMMAMAXAMXXASMMMMMMMMSMASMMMSMSXAAMXMSAAMAAAMAMAMSXMMXAAAMXMMMSAMAAASMMASXAXMMMXAXMMMMMSSSSMSSSXSAMMAXMMAS
SXSAASMMMAMAMXMSSMXMMSSMMMSMSAMAMMXMSXSXMSMXMXMAAMAXXAAAXASAMAAASXSMSSMMSMSSMMSMMAAXXMSMSSXSASAMMMSSMMMAXASAMSSXMSMMAAMXXXAAAAAAXMMASMSMXSAM
MASMMSXMMXSMMSAMXAXXXAAAXXAAXSMSXMAMXMAXMMMAXAMMMSSSSMSMMMSAMSSXSAMAAAAAXXAXXXAXXMMXMSAAXAASXMAMXMXMMXSAMXSMAAMAAAMAXSASXMMMMMMMMXSXMAXMAMMX
MAMAXXAXMAMAXSXSSSXAMSXMMMMSMAXAAXXSAXAXMASMSMSSMAXXXXAAAASMMMMAMAMMMMMMSMSASMSSXAAAMSMMMMMMXSAMAXMAAAMASXMXMAMAMMSSSXMAAMAXAAXMSAMXMAMMSSSM
MMSMMSSMMSSXMMAMAMAXMXMMXMAMXMMSSMASMSMMSASXAXAAMSSMSSSXMMXXAAMAMAMAMXAMAMMASMAAXSMMMXMAAMAMAMAMXSSMMMSXMAXMXMXSXXAMXAXMMMASMSSXSASAMXSXAAXM
AAAXAAAAXAMAAMSMAMXSMXMMAMXSAMMAAXAMXAMXMAMMAMMSXXAXMAMMSSMSSSSXSMSASMXSASAAXMXSXMAMXMSSXSAMASXMXXAAAAXASXMAXXAXMAMXSAMXSMMSAAMASMMMSMSAMSMS
MASMXXSMMMSXMXXMXMMAMAMSMMMSAMMSMMSSSMSSSSMASXXMMXMMMMMAAAXMAXXMAMSMXMASMSMSSXMXASAMXAAXXSXXMAAXXSSMMMSAMASXSMASASXXMSXAAAXMMMMAXAAXAMXSXMAM
MAMAMMXXMXAMMXSXSASAMAMAASMSXMAMASAMAAAMAMMXXXAMAASASXMMSSMMSSMXMASAXSXMAXAAMMAMXMAMXMMSXSAMXSSMAMXMAXMXMXMAXMXMAMASMMMMXMMXXXMAXMMSXSAXMMAM
MAXMXAMMSMSXMAXAXXSMMMSSSMAMMMSSMMASMMMMAMAAMSSMSXSASXMXAAXAAAMASMMAMSAMXMMMXASXSMSMSMAMAMXMAAAMXMAMXSAMXMMXMXSMSMAMAAAAXSMXSMMXSAAAAMMSXSXS
SSSSMMSAXAAMMSMMMMXMAXAXMMXMXMAAASAMXMASMSMAXMAAXAMMMASMSSMMSMMXMAAAMSAMAXAXXXMAXAAAAAAMAMXMMSXMASAXAMXMXMMMSAMAXMXSXSXSAAXAXAAAMMMMSMMMAMMM
XMAAAXXMMMMSAXAAAXSSXXAMMASMMXSXMXAMXSMMMAXMSMMMMMMASAMXAAXXXXSSSMXMMXASXSMSSMMAMSMSMSMSSXSAAAASXSMSMSAXAMXAMAMSMXMMAXAMXAMXSMMSSSMMAAAMAMAA
XMAMSSMMMXXMAXSSSMMASMASMASAAAXSXXASMSXXXAMMAMXSAASAMASMSMMXMAMAASXSXSXMAXMAMAMMMXAMAMAXAASMMSXMAXAAXSXSMSMXSAMASMMMSMXMSMSMSXMAAMSXSXMSMMMS
SXSXAAAXAXMMMXMAMAMAMXAMMXSMMMSAMSAMXMAXSAMXAMAMXMSASAMMMAMXMAMSMMSMAMXMASMASXMXSMMMAMAMMMMMAMAMXMMMXXMAXXMAMASXXXAAAMAMXAAAXMMMSMMMMXMAXAXX
AMXXSSMMMAMASMMAMXMASMXSSMMXSXMXMMAXAMAMSAMSAMASXMSXMASMXXXXMMMMAMAMAMMMMXMASAMAAAASXMAXXAXMAXMASAMXSAMXXXMASXMMSSMMSAMXMSMXMSXAAXAAMASASXSM
AMSMMAMAMASMXAMAMXMAMAMAXAMAMSAMXMXSSMAMMSMXAXAMMASXMXMAMSMMMSAMASXSXXAAAXMXMAMSMMMMMMMXSASMSXSMAMAMXMMMSSMASAAAMXSAMMSAMXSASXASXMMSMAMMMAAX
SXMASAMAXMXAMSMSMSMSMSAMSSMAMSSSMSMAAMMSAMXSSMSMSAMXXSMMMAAAXSASASXXMSSSSSMMMSMMASAMAMMAMAMAAAMXXXSASXMAMMAASMMMSAMXMAMASAMXMAXMAXSAMASXMSMM
MASMMMSSSMMSMMAMAAAXAMSXAASMXMASAAMSMMAMAMXAMAMAMXMSAMAAXMSMMMAMMXMMXAXXAAASAAAXMSAMASMASMMSMSMASMMAMAMSSMMMSXSAMASXMAMSMMMAMMMMXMSAMMXMAXAA
MAMAAMAMAMAMAMAMXMMMAMMMMMSMAMAMSMXMAMMSSMMXMAMMSMMXASMMMAAMMSASMMSAMMSXSSMMMSMMXSAMASMXMMAXMAMAXAXXMAMMAMAMXXMASAMXMAMMAMMAMAAXMASAMSXSASMS
MSXSMMXXAXASXMXSXXAMXAMXMAMXAMXXAXAXAMMXMASAMAMSAMXSMMASAMXSAAAAXASXSXSMAMXAAAAMXMASMXMAMMMMXAMMSMMSMXXSAMXMSMSAXAXMAMSMAMSMSXSXXXXXSAAMXSAX
AMAMXMSSMSXXMXAXAMXSSMMSMASMMSSSSSMSASAASXMASAMSASXMAXAMAMAMXSSMMXSASAMXMMSMSXSMMAMXASMMMAXSMSSXAASMASAMMMASAMSMXSASMSMXXMAXMAMMMMMMMMSMSMMM
XMAMAMAAMMXMSMMSSMXAAMAMXASAXSAAXAASAMMASAMMMMXSAMXXXMSSSMASAMAXSSMAMXMASAXAAAXASMXMXMASXMSMAMXMSXMASMASAMXXAMXAXAAAXAMSSMMSMAXMMAAAXAAXAAAS
XMASAMMMMAAMAAXAAMMSXMAXMMSAMMMMMSMMAMMMSAMASMAMAMSSSMAAASXMMMAMXMMMMASXSSMAMMMAASAXXSAMXSAMSMSAMXXXXSAMMASMSMMXMMMMSMSAAXAXXSMASMASXXSXSAAS
MXXXMSMSMSSSSSMMSMAMXSAXMXXMAAAXAAXSXMAMMAMASMXSAMAAAMMSMMXSMSMSXMAMSMAAXXMASXMXMSXSXMAMXSXMAMMMMMMMMMMSXAMAMAAMAMAAMMMMSMXSAXXAMXSAXXAAXMAS
SSXSAAASAMAAMXAAMMSSXMASMAAMSSSMSSXMSMAMSAMXSMMMAMMSMMXAMAMXAAAMMXAMAMMMSMSASMAAMSAMMMSAMXMSXXAAAAAXAAAMMSSMMMSASMMXSAMXMAMAMMMSAMXAAMMMXSAM
AAAMMMSMAMMXMMMMSAXMASAAMXMAAAAXAMXAASAMSAMXMXASAMAAASXSMSASMMSMASXSASAAAXMASMSAMXAMAAMAMXMASMSSSSSSMMXSAMSXAXMMMAAASXSAAAXAMXXMASMSMXMSAMXS
MMMMXMAXAMXXMASMMXMXMAMSAMXXMSMMMAMSMSSMSXMSMSXSXSSSSMAXXAMXMXXXMAMMXSMSSSMMMAMXSSXMMSSSMSMASXXXXMAAXAAMXMXMASXXSMMMSXSASASXSXAMSAAAXSXMASMA"""
