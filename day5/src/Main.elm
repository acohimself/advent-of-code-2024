{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2024/day/5

-}


module Main exposing (main)

import Browser
import Char
import Dict exposing (Dict, empty, get, update)
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (drop, filterMap, foldl, foldr, head, length, map, member, sortWith, sum)
import Maybe exposing (Maybe, withDefault)
import String exposing (lines, split, toInt, words)
import Tuple exposing (second)


title : String
title =
    "Day 5: Print Queue"


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


type alias Rules =
    Dict Int (List Int)


type alias Update =
    List Int


parse : String -> ( Rules, List Update )
parse s =
    let
        updateRuleList : Int -> Maybe (List Int) -> Maybe (List Int)
        updateRuleList new existing =
            case existing of
                Nothing ->
                    Just [ new ]

                Just e ->
                    Just (new :: e)

        parseRules rs =
            rs
                |> lines
                |> map (split "|")
                |> map (filterMap toInt)
                |> foldr
                    (\r rules ->
                        case r of
                            r1 :: r2 :: _ ->
                                Dict.update r2 (updateRuleList r1) rules

                            _ ->
                                rules
                    )
                    empty

        parseUpdates : String -> List Update
        parseUpdates us =
            us
                |> lines
                |> map (split ",")
                |> map (filterMap toInt)
    in
    case split "\n\n" s of
        r :: u :: _ ->
            ( parseRules r, parseUpdates u )

        _ ->
            ( empty, [] )


findMiddlePageNumber : Update -> Int
findMiddlePageNumber u =
    u
        |> drop (length u // 2)
        |> head
        |> withDefault 0



-- returns true if the rules are broken


checkRule : List Int -> List Int -> Bool
checkRule rest rulesList =
    case rulesList of
        r :: rs ->
            member r rest || checkRule rest rs

        [] ->
            False


checkPage : Rules -> Update -> Bool
checkPage r u =
    case u of
        p :: rest ->
            case get p r of
                Just rvs ->
                    checkRule rest rvs || checkPage r rest

                Nothing ->
                    checkPage r rest

        _ ->
            False


part1 : String -> Int
part1 input =
    let
        ( rules, updates ) =
            parse input

        breakingRules =
            map (checkPage rules) updates

        filterBroken : List Update -> List Bool -> List Update
        filterBroken u b =
            case ( b, u ) of
                ( False :: bs, good :: us ) ->
                    good :: filterBroken us bs

                ( True :: bs, _ :: us ) ->
                    filterBroken us bs

                _ ->
                    []
    in
    breakingRules
        |> filterBroken updates
        |> map findMiddlePageNumber
        |> sum


part2 : String -> Int
part2 input =
    let
        ( rules, updates ) =
            parse input

        breakingRules =
            map (checkPage rules) updates

        filterValid : List Update -> List Bool -> List Update
        filterValid u b =
            case ( b, u ) of
                ( False :: bs, _ :: us ) ->
                    filterValid us bs

                ( True :: bs, bad :: us ) ->
                    bad :: filterValid us bs

                _ ->
                    []

        sorter : Rules -> Int -> Int -> Order
        sorter r a b =
            case get a r of
                Nothing ->
                    LT

                Just rs ->
                    if member b rs then
                        GT

                    else
                        LT
    in
    breakingRules
        |> filterValid updates
        |> map (sortWith (sorter rules))
        |> map findMiddlePageNumber
        |> sum


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
    """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""


realData : String
realData =
    """79|97
51|89
51|74
74|65
74|63
74|53
41|94
41|54
41|55
41|26
95|51
95|23
95|54
95|33
95|86
55|78
55|94
55|68
55|74
55|87
55|44
94|89
94|65
94|16
94|23
94|33
94|83
94|46
68|75
68|74
68|33
68|97
68|23
68|19
68|86
68|47
63|64
63|26
63|44
63|47
63|72
63|94
63|78
63|62
63|25
47|96
47|51
47|52
47|46
47|23
47|45
47|33
47|11
47|58
47|88
45|11
45|26
45|54
45|86
45|96
45|65
45|35
45|23
45|88
45|32
45|89
54|31
54|23
54|19
54|75
54|71
54|28
54|98
54|14
54|88
54|87
54|62
54|69
31|37
31|72
31|78
31|41
31|55
31|47
31|68
31|16
31|73
31|44
31|81
31|79
31|25
64|65
64|45
64|83
64|68
64|94
64|55
64|26
64|89
64|47
64|78
64|72
64|95
64|41
64|97
11|63
11|28
11|81
11|53
11|25
11|79
11|58
11|31
11|98
11|55
11|14
11|62
11|32
11|88
11|69
19|64
19|83
19|95
19|88
19|79
19|52
19|55
19|31
19|72
19|63
19|73
19|41
19|58
19|37
19|94
19|28
15|71
15|46
15|98
15|11
15|86
15|54
15|88
15|74
15|51
15|23
15|33
15|35
15|96
15|45
15|65
15|47
15|19
53|64
53|86
53|83
53|72
53|62
53|25
53|45
53|37
53|95
53|73
53|44
53|41
53|16
53|81
53|94
53|63
53|15
53|55
35|54
35|37
35|75
35|11
35|46
35|88
35|97
35|87
35|53
35|96
35|31
35|71
35|19
35|23
35|81
35|28
35|63
35|25
35|14
14|25
14|32
14|83
14|72
14|81
14|73
14|63
14|78
14|64
14|44
14|28
14|31
14|69
14|94
14|95
14|41
14|62
14|55
14|37
14|79
23|75
23|37
23|25
23|64
23|87
23|52
23|98
23|63
23|31
23|88
23|32
23|62
23|11
23|81
23|19
23|73
23|28
23|96
23|46
23|41
23|69
44|89
44|23
44|45
44|97
44|26
44|33
44|75
44|54
44|19
44|65
44|86
44|96
44|15
44|16
44|47
44|35
44|51
44|46
44|11
44|74
44|71
44|68
26|11
26|31
26|75
26|32
26|19
26|89
26|23
26|58
26|88
26|54
26|53
26|98
26|74
26|52
26|46
26|65
26|71
26|69
26|97
26|35
26|33
26|96
26|87
98|62
98|41
98|31
98|72
98|78
98|68
98|81
98|37
98|79
98|25
98|88
98|44
98|32
98|95
98|52
98|63
98|53
98|64
98|28
98|73
98|83
98|55
98|94
98|69
73|44
73|72
73|54
73|86
73|15
73|78
73|35
73|55
73|33
73|74
73|45
73|83
73|64
73|89
73|47
73|95
73|65
73|51
73|79
73|94
73|26
73|16
73|68
73|41
81|73
81|83
81|78
81|45
81|15
81|72
81|26
81|65
81|86
81|94
81|74
81|55
81|68
81|62
81|44
81|89
81|16
81|95
81|33
81|47
81|51
81|41
81|64
81|79
37|79
37|62
37|95
37|55
37|72
37|83
37|51
37|15
37|16
37|81
37|25
37|28
37|26
37|68
37|86
37|45
37|47
37|44
37|64
37|78
37|73
37|63
37|94
37|41
69|62
69|53
69|63
69|37
69|31
69|41
69|72
69|83
69|28
69|44
69|81
69|95
69|94
69|78
69|16
69|47
69|25
69|55
69|73
69|64
69|68
69|79
69|32
69|15
89|14
89|97
89|65
89|31
89|52
89|19
89|11
89|71
89|46
89|96
89|75
89|58
89|25
89|98
89|23
89|53
89|63
89|69
89|88
89|32
89|37
89|54
89|35
89|87
62|78
62|94
62|74
62|68
62|35
62|33
62|41
62|65
62|45
62|16
62|15
62|86
62|79
62|26
62|55
62|72
62|95
62|44
62|51
62|73
62|47
62|83
62|89
62|64
58|83
58|55
58|72
58|14
58|73
58|25
58|98
58|69
58|52
58|94
58|78
58|81
58|28
58|64
58|37
58|53
58|41
58|62
58|32
58|31
58|88
58|95
58|63
58|79
46|19
46|32
46|81
46|52
46|58
46|72
46|62
46|37
46|14
46|31
46|28
46|63
46|75
46|96
46|87
46|88
46|25
46|69
46|64
46|73
46|41
46|53
46|98
46|11
97|63
97|75
97|87
97|96
97|73
97|98
97|37
97|32
97|19
97|62
97|31
97|14
97|88
97|69
97|71
97|25
97|46
97|23
97|53
97|81
97|11
97|28
97|58
97|52
78|44
78|47
78|26
78|45
78|33
78|97
78|65
78|89
78|51
78|16
78|71
78|74
78|96
78|35
78|15
78|19
78|46
78|87
78|54
78|68
78|11
78|86
78|75
78|23
88|95
88|55
88|52
88|28
88|32
88|44
88|64
88|63
88|62
88|83
88|79
88|81
88|37
88|16
88|31
88|73
88|69
88|94
88|41
88|72
88|25
88|53
88|68
88|78
52|78
52|16
52|37
52|81
52|79
52|62
52|94
52|55
52|68
52|31
52|32
52|44
52|73
52|72
52|15
52|63
52|64
52|83
52|69
52|95
52|41
52|53
52|28
52|25
96|37
96|52
96|69
96|14
96|79
96|81
96|64
96|75
96|28
96|31
96|88
96|41
96|53
96|19
96|62
96|11
96|73
96|32
96|63
96|98
96|55
96|58
96|25
96|72
72|33
72|16
72|95
72|51
72|89
72|94
72|47
72|74
72|86
72|83
72|97
72|23
72|79
72|44
72|78
72|71
72|45
72|35
72|15
72|55
72|26
72|65
72|54
72|68
87|52
87|81
87|19
87|62
87|75
87|37
87|11
87|31
87|32
87|28
87|53
87|63
87|64
87|25
87|96
87|58
87|88
87|73
87|14
87|69
87|98
87|72
87|41
87|79
75|31
75|88
75|28
75|81
75|14
75|58
75|98
75|72
75|37
75|64
75|55
75|95
75|32
75|25
75|19
75|53
75|63
75|79
75|41
75|94
75|73
75|69
75|52
75|62
32|15
32|95
32|64
32|83
32|31
32|51
32|79
32|44
32|53
32|16
32|55
32|72
32|47
32|25
32|73
32|41
32|94
32|62
32|68
32|37
32|28
32|81
32|63
32|78
71|64
71|32
71|58
71|98
71|28
71|53
71|87
71|14
71|46
71|11
71|73
71|25
71|69
71|63
71|81
71|23
71|62
71|88
71|37
71|19
71|52
71|75
71|31
71|96
33|98
33|96
33|58
33|97
33|89
33|65
33|35
33|87
33|23
33|31
33|14
33|52
33|74
33|54
33|37
33|32
33|71
33|46
33|75
33|53
33|19
33|88
33|69
33|11
65|88
65|63
65|19
65|58
65|14
65|31
65|71
65|96
65|37
65|11
65|98
65|23
65|75
65|32
65|35
65|87
65|54
65|52
65|69
65|53
65|25
65|28
65|46
65|97
86|52
86|89
86|71
86|69
86|54
86|26
86|74
86|98
86|97
86|65
86|32
86|58
86|96
86|19
86|87
86|33
86|31
86|35
86|75
86|23
86|14
86|88
86|46
86|11
25|95
25|86
25|33
25|74
25|55
25|26
25|51
25|28
25|72
25|41
25|68
25|47
25|45
25|79
25|83
25|81
25|73
25|78
25|64
25|16
25|62
25|15
25|44
25|94
83|35
83|96
83|46
83|71
83|47
83|11
83|45
83|26
83|75
83|86
83|89
83|78
83|68
83|65
83|97
83|16
83|23
83|87
83|51
83|44
83|54
83|15
83|33
83|74
28|45
28|89
28|64
28|74
28|72
28|44
28|79
28|16
28|83
28|15
28|68
28|81
28|41
28|62
28|95
28|94
28|78
28|86
28|26
28|51
28|33
28|47
28|73
28|55
16|75
16|11
16|54
16|98
16|96
16|23
16|86
16|87
16|33
16|51
16|65
16|45
16|15
16|19
16|14
16|26
16|74
16|97
16|35
16|46
16|89
16|58
16|71
16|47
79|95
79|71
79|47
79|33
79|26
79|68
79|35
79|86
79|51
79|65
79|15
79|23
79|94
79|74
79|89
79|78
79|46
79|44
79|83
79|54
79|45
79|16
79|55
51|96
51|26
51|11
51|33
51|46
51|58
51|86
51|87
51|52
51|71
51|88
51|54
51|14
51|97
51|75
51|19
51|45
51|98
51|65
51|69
51|23
51|35
74|46
74|88
74|37
74|98
74|96
74|31
74|32
74|89
74|52
74|69
74|19
74|87
74|54
74|58
74|71
74|23
74|11
74|97
74|75
74|14
74|35
41|78
41|16
41|44
41|51
41|89
41|33
41|72
41|15
41|47
41|35
41|65
41|71
41|68
41|83
41|79
41|97
41|45
41|86
41|95
41|74
95|11
95|26
95|97
95|96
95|83
95|71
95|89
95|78
95|65
95|68
95|15
95|44
95|46
95|45
95|87
95|16
95|35
95|47
95|74
55|23
55|89
55|54
55|35
55|65
55|26
55|95
55|46
55|71
55|47
55|33
55|16
55|83
55|15
55|86
55|51
55|97
55|45
94|74
94|68
94|35
94|44
94|95
94|15
94|78
94|54
94|26
94|96
94|71
94|47
94|86
94|45
94|97
94|87
94|51
68|11
68|46
68|58
68|26
68|71
68|14
68|54
68|15
68|51
68|96
68|89
68|45
68|16
68|35
68|65
68|87
63|55
63|15
63|86
63|16
63|73
63|79
63|68
63|41
63|28
63|51
63|81
63|83
63|45
63|33
63|95
47|75
47|19
47|54
47|74
47|35
47|26
47|65
47|87
47|86
47|14
47|89
47|71
47|97
47|98
45|74
45|71
45|58
45|75
45|87
45|97
45|69
45|14
45|33
45|46
45|19
45|52
45|98
54|52
54|11
54|53
54|97
54|96
54|58
54|37
54|46
54|81
54|25
54|32
54|63
31|15
31|53
31|45
31|28
31|94
31|63
31|64
31|83
31|62
31|51
31|95
64|15
64|79
64|44
64|33
64|35
64|74
64|54
64|51
64|86
64|16
11|19
11|41
11|72
11|64
11|94
11|37
11|52
11|73
11|75
19|32
19|14
19|81
19|98
19|25
19|53
19|69
19|62
15|14
15|58
15|87
15|75
15|97
15|89
15|26
53|47
53|79
53|68
53|28
53|51
53|78
35|52
35|98
35|32
35|69
35|58
14|98
14|53
14|88
14|52
23|58
23|53
23|14
44|87
44|58
26|14

81,41,95,78,68,16,47,51,45
79,94,95,78,44,68,15,47,45,86,26,89,65,35,23
78,16,15,47,51,45,86,89,65,35,97,23,96
11,75,19,58,98,88,52,69,32,63,28,81,73,41,79
44,15,45,74,89,54,71,87,19
69,32,53,63,28,81,62,73,72,79,94,95,83,78,44
62,64,41,72,79,94,95,83,44,68,16,15,47,51,45,86,26,33,74,89,65
37,63,25,28,81,73,64,41,72,79,55,95,83,78,44,68,16,47,51,45,86
44,68,16,15,47,51,45,26,33,74,89,23,46,87,11,75,19
53,44,51,25,95,68,55,37,47,62,31
31,58,63,23,52
75,19,58,14,88,52,69,31,53,63,25,64,41,72,79,55,94
88,63,62,75,41,98,28,81,11,58,25,52,87,19,73,46,53
88,31,87,73,69,11,52,71,62,28,58,23,63,14,37,81,25,96,53
31,58,73,28,32,94,72,55,53,79,41,52,88,64,63,62,19,69,37,98,14
55,94,95,83,78,47,51,45,26,74,89,65,35,54,46
96,63,75,46,62,87,28,52,11,98,19,71,32,25,97,14,58
31,53,25,81,79,68,15,47,51
15,33,89,65,55,74,72,78,73,51,68,26,79,64,86,47,16,95,45
63,25,28,81,62,73,64,41,79,55,94,95,83,78,44,68,16,15,47,51,45,86,26
64,45,26,47,63,86,81,73,68,44,83,16,78,15,25,62,41,28,55
74,44,54,94,47,35,87,78,68,65,26,51,46,89,86,16,33
65,15,33,68,55,97,78,83,45,94,95,74,51,47,26,71,72
54,46,96,11,14,98,88,32,31,53,81
41,78,26,62,15,79,65,74,86,89,68,47,55
88,87,54,33,45,96,58,74,14,71,11,69,75,52,98,26,23
23,11,14,65,74,31,52,87,98,89,54,33,26,75,96,46,35,69,19,32,71,58,88
88,14,64,72,69,75,28,19,58,87,11,31,41,53,81,37,73,63,32
97,71,87,75,14,98,52,32,31,53,37,25,62
68,53,83,72,81,32,79,64,37,63,31,62,41,44,25
26,54,46,75,19,88,52,69,31
19,73,52,31,63,11,98,14,96,32,37,81,69,87,25,75,23,58,71
88,52,31,63,64
28,63,41,72,16,86,79,47,94,51,62,25,64,26,15,95,45,78,73,83,68
89,35,54,97,46,87,75,58,98,52,69,31,63
37,54,11,71,52,63,75,81,98,46,97,23,69,14,32,58,19,96,87
41,72,79,55,94,95,83,78,16,15,47,51,86,26,74,89,35,54,97
26,33,69,31,58,88,11,96,54,23,87
58,16,96,87,51,68,74,54,47
81,62,73,41,72,55,83,78,44,16,15,47,26,74,89
69,62,52,41,32,28,98,11,72,64,81,88,55
16,15,47,51,45,86,26,33,74,65,35,54,97,71,23,46,87,96,11,19,58
69,53,58,46,75,88,54,25,19,97,31,98,96,14,37
46,96,11,75,19,58,14,98,88,52,69,32,31,53,37,63,25,28,81,62,73,64,41
78,44,68,15,47,51,45,86,26,33,74,89,65,35,54,97,71,23,46,87,96,11,75
69,32,37,81,55
41,72,79,94,95,83,68,16,47,51,45,86,33,74,35,54,97
23,11,32,31,53,25,64
69,32,31,63,25,64,79,44,68
81,62,72,79,95,78,47
74,89,65,35,54,97,71,23,46,87,96,11,75,19,58,98,88,52,69,32,31,53,37
75,74,33,89,86,58,97,47,26,98,65,87,19,45,88,11,23,35,96,14,71
96,11,75,19,58,14,98,69,32,31,53,63,25,28,81,73,41,72,79
37,52,88,96,19,53,31,14,11,79,81,69,58
55,47,89,51,26,78,71,33,74,65,44,15,83,86,54,35,68,72,16
55,94,95,83,78,44,68,16,15,47,51,45,86,26,33,74,89,35,54,97,71,23,46
45,26,68,74,83,79,41,72,55,44,89,64,51,65,94,54,47,86,16
28,81,62,73,72,55,95,83,78,16,15,47,45,33,74
33,47,75,15,97,44,16,35,46,51,45,78,54
95,68,45,74,89,65,96
78,44,16,15,47,51,26,33,74,89,35,54,97,71,23,46,87,11,75
32,53,25,73,41
62,31,14,69,52,53,58,32,25
54,97,87,86,26,19,23,98,58,52,45,75,33,14,74,46,89,35,71,11,96,51,88
35,69,19,75,14,98,37,11,63
14,53,64,46,19,25,75,62,98,63,69,31,23,96,52,37,88
46,96,58,98,88,69,32,25,28,62,73,64,41
14,96,98,45,19,15,33
35,54,71,46,87,96,11,14,52,69,32,31,53,63,25
95,83,78,44,68,16,15,47,51,45,86,26,33,74,89,65,35,54,97,71,46,87,96
68,26,96,65,51,54,44,74,35,46,86,15,16,11,19,47,75,71,87
37,63,25,81,62,55,94,83,15,51,86
46,97,32,23,71,87,88,65,96,35,89,58,54
74,94,26,71,55,54,65,86,15,97,78,45,68,47,83,23,79,95,16,89,51
79,41,63,37,11,73,96,53,31
75,98,65,54,97,37,58,69,11,87,96,32,35,23,31,89,71,74,46
86,26,33,89,65,97,87,19,98,69,32
94,95,83,78,86,26,33,71,87
15,33,26,83,47,55,41,54,65,86,35,72,95,79,89,44,51,16,74,45,68,97,78
87,54,74,89,46,97,47,71,11,23,26,14,35,65,96,58,75,16,15,51,33,86,19
88,69,32,53,37,81,72,79,55
19,52,53,62,64,79,95
51,45,44,86,89,65,97,19,16,47,46,11,75,68,15,33,23
47,28,53,95,16,73,72,41,68,62,37,44,31,51,64,79,15,83,55,25,78,63,81
46,96,11,75,58,98,88,32,53,37,63,25,28,81,62,73,41
81,95,94,16,32,47,28
52,69,53,37,63,25,28,81,73,64,41,72,79,55,94,95,78,68,16
55,94,83,44,26,97,46
83,68,47,74,89,87,11
73,11,37,63,25,88,64,58,55,19,28
33,74,68,44,95,51,94,65,64,41,78,72,15,73,35
94,46,71,83,47,23,55,26,54
35,96,54,25,46,71,69,11,88,58,52,53,97,32,14,31,23,75,98,28,37,63,87
58,32,63,28,81,72,55,95,83
64,41,72,79,55,83,78,44,68,15,47,51,45,26,33,74,89,65,35
64,98,32,69,62,73,63,72,79,81,28,14,58,75,52,19,31,11,96,41,25
41,15,62,44,94,45,72,26,65,73,83,51,47,89,86,55,68,33,16
87,96,19,58,98,52,31,53,37,81,73,64,72
31,53,37,71,81,58,28,69,75,98,96,54,14,46,52,97,11
54,97,71,23,87,19,58,14,69,31,53,25,81
98,19,26,47,71,33,86,23,75,87,74,11,89,51,35,65,96,45,58,15,97,14,54
51,45,86,33,74,89,65,54,97,96,75,58,52
73,95,68,44,55,78,72,37,94,47,83
74,65,71,11,14
23,89,44,86,68,74,26,83,54,15,96,51,46,71,33,45,47,16,11
89,65,54,71,23,46,87,11,98,88,69,32,31,37,63
16,15,47,51,45,86,26,74,89,65,35,54,97,71,23,46,87,96,11,75,14
73,41,95,31,88,63,64,53,25,14,52,62,32,83,98,37,94,69,28,78,72,55,81
53,37,72,28,63,69,55,11,31
98,26,58,74,75,87,45,65,11,33,89,96,51,52,86,14,54,23,71,19,88
95,83,44,16,15,47,51,45,86,26,33,74,89,65,35,54,97,71,23,87,96
78,62,55,47,26,44,73
83,78,16,15,47,51,86,33,65,35,54,97,71,23,87,96,11
33,35,23,74,58,46,52,88,75,32,11
45,33,74,97,23,88,69
89,97,71,23,46,96,75,19,58,14,98,88,52,69,32,31,53,37,63
37,62,72,79,78,68,16,15,45
44,83,37,86,72,25,28,95,47,73,16
51,19,54,74,68
33,55,65,89,47,72,54,16,45,94,95,35,51,97,71
64,41,72,79,55,94,95,83,78,44,68,16,15,51,86,33,74,65,35
25,28,81,73,64,41,72,79,55,94,95,83,78,68,16,15,47,51,45,26,33
47,33,65,35,23,58,88
52,11,96,46,69,62,81,23,87,14,58,31,32
83,78,44,15,47,86,33,74,65,97,71,23,46,96,11
98,52,69,32,31,37,28,81,62,79,94,83,44
28,98,41,75,64,72,14,87,81,63,96,37,32,19,31,88,53,69,58
28,88,78,81,37,63,73,14,25
63,25,41,44,51
35,31,46,88,54,65,63,14,37,11,19,53,89,87,58,97,23,96,71,52,75
53,28,73,44,68,47,45
16,15,51,45,86,26,33,74,89,65,35,54,97,71,23,46,87,96,11,75,19,58,14
62,64,72,79,94,83,78,44,51,45,86,33,89
14,98,52,69,32,31,53,37,63,25,28,62,73,64,41,72,79,55,95,83,78
16,71,65,96,15,89,86,54,74,45,51,47,46,35,19,75,87,14,33
94,44,55,63,62,26,64
32,31,63,25,73,72,79,94,95,78,68,16,15
26,15,78,28,55,41,94,81,16,68,64,86,72,79,51,44,74
74,65,68,16,45,94,83,51,15,64,72,41,89,95,44,55,73,33,26,47,86,35,78
45,44,41,47,74,78,54,79,64,26,51,86,15,65,89,72,94,95,68,16,33,35,83
51,41,94,55,26,79,74,28,45,33,47,62,64,78,16,15,44
45,26,89,35,97,87,96,75,19,58,88,52,69
94,95,62,53,25,41,83
54,97,71,46,87,58,14,88,52,31,53
69,31,53,37,63,25,28,81,62,73,64,41,72,55,94,95,83,44,68,16,15
44,45,86,68,78,55,79,81,51,62,47,64,72,15,73,41,16,83,94,28,33
78,16,15,26,74,89,35,71,23,96,75
35,68,26,23,89,71,33,16,86,74,79
71,11,73,31,23,25,52,88,75,98,87,81,32,19,14,46,62
89,54,97,71,23,46,87,11,58,14,98,88,69,32,53,37,63
96,75,52,53,37,62,79
68,95,86,89,35,54,78,26,64
44,86,51,72,55,26,78,83,47,64,65,73,62,16,89
73,69,37,11,62,75,28,32,81,23,63,71,96,87,31
64,41,72,79,95,47,45,74,89,65,54
97,88,54,23,87,26,71,52,19,58,75,11,33,31,46,35,65,14,98
69,32,31,53,63,25,28,81,62,73,64,41,79,55,94,95,83,78,44,16,15
95,16,15,86,26,33,89,54,97,71,23,87,96
53,62,94,83,44,47,45
53,37,73,79,55,94,95,68,16,15,47,51,45
79,55,51,41,64,68,63,73,81,16,26,25,95,45,15
54,97,71,23,46,87,96,19,58,98,88,52,69,32,31,53,37,63,25,28,81
31,69,54,46,23,11,71,14,87,58,74,89,52,98,53,32,19,75,96,97,65,35,33
58,52,87,98,88,37,46,35,19,25,14,69,96
33,65,54,71,23,75,58,14,69
41,72,79,55,44
74,51,15,83,47,86,35,65,97,44,41,95,16,33,68,26,45,55,54,89,94,72,79
31,53,63,72,55,95,83,78,44,16,15,47,51
72,63,62,51,37,78,44,41,25,53,95,28,15,55,47,45,81,94,64,73,16
45,33,74,89,65,87,69
63,83,78,16,15
45,33,54,23,96,89,15,68,16,19,11,97,51,71,87,58,65,26,46
41,95,83,78,68
32,19,75,14,52,23,96,64,63,31,81
88,52,69,32,31,53,37,63,25,28,81,62,73,41,72,79,55,94,95,83,78,44,68
25,53,96,79,52,73,32,11,19
72,95,28,64,37,55,51,94,83,73,45,25,16,79,15
41,95,31,51,16,44,94,53,63
19,62,75,73,31,98,71,53,96,28,52,25,81
46,87,96,75,19,58,14,98,52,69,32,37,63,25,81,62,73,64,41
79,94,83,51,89,54,23
78,68,47,26,97,71,75
53,28,55,41,83,88,81,95,78,94,62,63,72
98,88,52,32,31,53,37,63,28,73,64,72,79,94,95,83,44
11,75,19,58,14,98,88,32,31,53,37,63,25,28,81,62,64,41,72,79,55
33,35,46,96,11,75,58,14,88,52,53
83,95,73,47,25,81,94,16,37,53,62,68,64
97,71,23,46,87,96,11,75,58,14,98,88,52,69,32,31,53,37,63,25,28,81,62
33,46,47,75,16,45,58,35,74,11,23,14,97,15,71,86,54,51,96,87,26,19,65
33,74,65,54,23,46,87,96,11,75,14,98,88,31,53
78,44,68,51,86,26,74,35,54,97,23,87,75
86,96,35,16,51,11,68,83,97,54,47,89,87,23,78"""
