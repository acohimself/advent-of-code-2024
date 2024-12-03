{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2024/day/2

-}


module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import List exposing (filter, head, length, map, reverse, sort, sum, tail)
import Maybe exposing (Maybe, withDefault)
import Regex
import String exposing (dropLeft, fromList, lines, startsWith, toInt, uncons, words)

title : String
title = "Day 2: Red-Nosed Reports"

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
    = InputFrequencyText String
    | Solve
    | LoadFromCache


type Direction
    = Inc
    | Dec


part1 : String -> Int
part1 input =
    input
        |> lines
        |> map words
        |> map (map toInt)
        |> map (map (withDefault 0))
        |> map solve1
        |> sum


solve1 xs =
    let
        numbersAreClose numbers =
            case numbers of
                n1 :: n2 :: ns ->
                    let
                        d =
                            abs (n1 - n2)
                    in
                    if (d > 0) && (d < 4) then
                        numbersAreClose (n2 :: ns)

                    else
                        0

                [ _ ] ->
                    1

                [] ->
                    0
    in
    if sort xs == xs || sort xs == reverse xs then
        numbersAreClose xs

    else
        0


part2 : String -> Int
part2 input =
    input
        |> lines
        |> map words
        |> map (map toInt)
        |> map (map (withDefault 0))
        |> Debug.log "I"
        |> map solve2
        |> Debug.log "S"
        |> filter (\x -> x)
        |> length


solve2 input =
    let
        checkDistanceAndDirection : Int -> Int -> Direction -> Bool
        checkDistanceAndDirection x1 x2 d =
            case d of
                Inc ->
                    (x2 - x1 > 0) && (x2 - x1 < 4)

                Dec ->
                    (x1 - x2 > 0) && (x1 - x2 < 4)

        proccesReport : Bool -> Direction -> List Int -> Bool
        proccesReport dampener direction report =
            case report of
                r1 :: r2 :: rs ->
                    case checkDistanceAndDirection r1 r2 direction of
                        True ->
                            proccesReport dampener direction (r2 :: rs)

                        False ->
                            if dampener then
                                proccesReport False direction (r1 :: rs)

                            else
                                False

                [ r1 ] ->
                    True

                [] ->
                    True
    in
    proccesReport True Inc input
        || proccesReport True Dec input
        || proccesReport True Inc (reverse input)
        || proccesReport True Dec (reverse input)


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputFrequencyText text ->
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
                        , onInput InputFrequencyText
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


inputString : String
inputString =
    """27 29 30 33 34 35 37 35
51 53 54 55 57 60 63 63
87 90 93 94 98
41 42 45 47 49 51 53 58
23 26 23 24 27 28
32 33 36 37 34 36 39 37
12 13 11 14 14
84 87 88 87 91
54 55 53 56 62
71 73 74 74 75 76 77
84 87 90 90 89
43 46 49 52 55 55 55
21 23 25 28 30 30 33 37
20 22 22 24 31
42 44 45 46 47 51 52 54
66 67 69 70 74 75 74
4 6 10 11 14 17 18 18
50 52 56 59 63
64 67 68 71 75 77 83
17 20 22 29 32 33 35
81 84 89 92 94 92
11 14 15 17 20 27 27
20 22 28 30 31 35
54 55 57 60 67 74
51 50 51 52 54
55 53 55 57 59 60 57
34 33 35 37 39 42 43 43
23 22 23 26 28 31 34 38
6 4 7 10 12 13 15 21
29 27 28 27 30 32 35 36
83 80 83 85 88 85 87 86
75 74 76 78 75 75
51 50 52 54 57 55 56 60
71 69 72 70 75
47 46 48 48 50
64 63 65 65 63
88 87 88 90 90 91 91
69 68 68 70 74
79 77 77 79 86
8 5 8 12 14 17
21 19 20 24 26 25
36 34 38 39 39
77 75 76 80 84
64 63 64 68 74
40 38 43 44 46 48 49
86 83 84 87 88 91 98 96
40 37 40 47 50 53 53
44 41 46 47 51
7 4 7 9 11 18 24
55 55 58 59 62 64
13 13 14 16 13
61 61 63 66 68 68
91 91 92 94 98
9 9 10 11 13 15 18 25
21 21 23 25 22 25
92 92 95 93 92
53 53 56 55 55
57 57 60 59 60 64
48 48 45 48 50 56
5 5 6 6 9
37 37 38 41 41 44 46 44
87 87 87 90 90
51 51 51 54 58
79 79 79 82 83 86 88 95
71 71 72 73 77 78
41 41 43 47 48 50 49
55 55 59 62 62
26 26 29 33 35 36 40
52 52 56 58 65
16 16 19 24 25
85 85 86 89 92 93 98 96
8 8 11 16 16
87 87 93 94 98
12 12 15 18 23 24 30
45 49 50 51 54 55
51 55 58 60 61 58
59 63 66 69 70 70
25 29 32 34 37 40 44
13 17 19 22 25 28 29 34
69 73 71 73 74
3 7 4 5 4
40 44 47 50 49 52 55 55
49 53 56 58 57 59 63
48 52 55 57 60 59 62 68
60 64 67 67 68 71 74
4 8 11 13 13 14 17 14
17 21 22 22 23 26 26
32 36 36 39 43
32 36 38 41 41 48
24 28 32 34 35 36
13 17 19 23 24 26 27 24
13 17 20 24 26 26
8 12 16 18 20 24
70 74 75 77 81 83 88
8 12 13 14 16 23 26
47 51 53 54 59 60 57
3 7 12 14 16 16
21 25 30 31 35
32 36 38 43 50
40 46 48 50 52 53
5 10 13 16 19 16
11 16 18 21 22 23 23
25 31 33 34 35 36 38 42
35 41 42 44 51
83 90 91 92 90 93 95
84 90 93 92 93 96 98 97
65 70 67 68 71 73 73
63 69 70 68 69 73
57 64 62 65 67 72
21 26 28 29 31 31 34
38 44 47 47 45
77 84 84 86 88 89 89
67 73 75 75 79
2 9 12 12 14 15 20
11 16 19 23 26
39 45 49 52 49
74 81 85 86 86
44 49 51 55 59
33 40 41 45 52
48 55 57 64 67 70
73 80 83 84 87 92 93 91
36 43 45 47 49 55 55
50 57 59 61 66 67 71
31 36 39 40 43 48 54
60 57 54 52 49 50
80 78 77 75 74 73 71 71
61 59 58 55 52 48
72 70 69 66 64 63 56
14 11 8 9 8 6
14 11 9 8 5 2 4 5
91 89 88 89 86 85 82 82
11 8 10 7 3
20 18 20 18 13
41 39 38 35 35 33 30
82 81 80 80 83
54 53 53 52 49 49
90 88 85 83 83 79
80 79 79 76 69
23 22 21 17 16
60 59 57 55 52 48 51
43 41 37 35 35
68 66 63 60 57 53 50 46
48 45 44 40 35
54 51 50 49 46 43 37 34
69 67 64 62 59 56 49 52
24 21 14 11 10 8 7 7
52 49 42 41 37
19 18 17 11 9 2
64 65 62 59 58 57
87 90 88 86 88
56 58 55 53 52 50 49 49
23 26 25 24 20
46 47 44 41 34
22 25 28 27 24
35 37 35 32 31 33 30 32
14 15 16 14 12 12
70 73 71 73 70 69 67 63
83 85 86 83 78
68 70 68 66 66 63 60 58
95 96 95 95 93 94
19 22 19 18 18 15 15
14 15 12 12 9 5
90 93 93 92 90 88 82
30 32 30 28 25 22 18 17
81 84 82 78 80
55 58 55 51 48 45 45
47 49 46 43 39 36 32
46 49 48 45 43 42 38 33
16 17 10 9 7 5 3 2
11 13 10 5 3 2 1 4
77 79 76 74 71 68 63 63
53 56 50 47 46 42
39 42 35 33 30 27 21
62 62 60 59 58 56 55
69 69 68 65 63 66
87 87 86 84 82 80 78 78
71 71 68 65 61
17 17 14 11 10 8 2
13 13 10 8 10 9 6 3
39 39 37 40 39 36 33 36
73 73 71 68 66 63 64 64
14 14 15 12 10 9 5
33 33 32 31 32 30 28 21
78 78 77 77 75 74 72
86 86 85 85 84 81 82
3 3 2 2 2
91 91 88 85 85 82 81 77
93 93 92 92 89 87 80
47 47 43 42 41 39
21 21 18 16 13 11 7 8
50 50 47 45 44 40 40
20 20 18 14 11 7
64 64 63 60 56 55 49
66 66 65 60 57
63 63 56 55 54 57
21 21 15 12 12
82 82 80 79 74 72 68
42 42 40 35 30
59 55 52 49 47 44
25 21 19 16 17
57 53 52 51 49 48 48
59 55 52 51 48 45 41
28 24 22 21 16
92 88 86 89 87 86 83 82
38 34 35 34 32 34
30 26 29 28 25 22 22
29 25 24 22 23 20 16
93 89 88 86 88 86 83 77
65 61 61 59 58 55
46 42 39 39 42
90 86 86 83 80 78 78
41 37 34 32 31 30 30 26
21 17 16 16 15 12 6
55 51 49 45 43
30 26 23 21 20 16 19
92 88 87 85 81 81
42 38 35 31 30 26
75 71 70 69 68 64 58
74 70 64 62 60
34 30 28 21 23
98 94 93 87 87
30 26 25 20 19 15
57 53 50 43 42 40 39 32
92 86 85 83 80 79 76
99 94 92 91 89 88 85 87
27 21 20 18 15 15
73 67 64 62 61 57
97 91 90 88 85 83 78
90 85 82 79 81 79 76 73
75 68 65 66 65 66
19 14 11 9 10 7 6 6
63 56 54 52 49 48 51 47
72 66 65 63 65 64 62 55
28 21 19 16 16 13
72 66 66 63 65
29 24 24 23 21 21
70 65 64 64 62 60 57 53
26 20 18 18 13
99 94 90 89 86 84 81
21 14 11 9 5 4 7
77 71 67 66 64 64
26 20 18 14 12 11 7
82 77 76 75 74 70 68 63
39 34 27 24 21
20 14 13 8 5 4 1 2
61 54 53 48 45 45
22 16 15 8 4
52 45 42 40 34 29
91 94 97 96 95 92 90
55 54 54 56 58 58
51 51 53 54 56 60 63 61
36 38 40 45 46 49 48
86 91 88 91 93 97
80 76 73 70 66 64 67
51 48 48 50 53 57
71 71 69 65 63 63
93 93 91 92 93
61 64 63 65 66 70
67 68 71 74 81 82 84 90
7 12 14 16 20 22 24 21
5 3 4 6 9 10 10 17
65 66 65 65 61
56 59 60 64 65 69
68 69 68 71 74 77
93 96 94 93 89 91
8 7 8 9 12 11 15
80 73 75 74 71
75 71 69 66 64 58 57 58
23 21 20 23 21 16
53 56 54 51 51 54
72 77 83 85 88 89 91
55 51 49 46 46 43 42
17 13 9 6 3 3
30 26 20 17 15 12 9 9
67 67 74 75 76 78
90 88 85 84 81 77 75 74
7 7 14 16 20
62 66 73 75 73
11 13 16 17 18 19 26
34 27 25 28 26 23 20 15
20 18 21 23 26 29 33 37
6 11 9 12 14 15 12
71 73 72 69 66
51 54 53 52 52 45
81 88 85 88 88
16 12 11 10 9 9
69 69 66 63 59 56 52
80 84 86 93 93
30 24 22 15 13 12 9
99 99 94 93 92 94
61 58 57 56 56 52
85 89 87 89 92 97
57 54 54 52 46
45 40 37 34 32 29 27 30
66 64 58 56 55 52 52
37 44 45 50 53 54 51
76 70 68 65 65 64 61 61
69 69 67 65 66 63 66
15 19 22 26 28 30 34
7 7 8 9 11 14 18 22
11 15 17 17 23
70 64 63 62 60 60 57 50
27 27 30 32 38 40 41 47
42 40 36 35 34 32 26
23 22 24 26 26 28 31 34
53 57 57 60 61 64 66 70
57 57 60 61 64 67 68 71
61 61 58 57 52 49 49
45 43 41 39 32 26
79 83 85 88 89 92 94 94
7 7 9 8 11 11
79 79 78 76 78
37 35 36 39 41 42 44 44
78 78 79 77 81
52 55 54 51 50 43
80 82 85 88 90 87 88 88
88 84 84 82 79 76 72
35 41 43 45 47 48 55 59
59 59 60 59 57 57
45 45 47 46 43 40 36
50 44 40 39 36 34 32 28
57 61 60 62 64 65 67
75 71 69 66 68 66 61
71 67 66 65 63 65 64
34 29 28 26 24 22 25 27
32 32 29 29 29
45 52 53 55 55
60 57 58 62 65 63
22 16 15 13 12 9 7 3
39 43 44 47 50 53 56 57
38 42 44 46 43
53 50 52 59 58
22 21 17 15 14 11 11
61 68 69 68 71 74 79
85 83 83 81 81
52 56 58 65 69
82 76 70 67 65 63 56
57 64 67 70 73 73 74 81
10 10 10 8 6 4
63 65 63 62 58 55 55
49 48 47 46 43 42 41
44 46 47 50 52 55 58
16 18 21 24 26
35 36 38 41 42 45 48
79 77 76 73 70 69 67
20 17 16 14 11
48 46 43 40 37
55 58 61 62 65 67
89 88 87 84 83 80 78
69 71 73 75 76
31 28 25 24 22 21 18 15
34 31 29 27 25 22 20 17
58 55 52 49 47 44 41
24 25 27 29 31 33 36 38
92 90 89 87 85 83
9 12 15 18 20
78 77 75 74 71 68
20 17 16 13 12 9 7
18 15 12 10 8
85 82 81 78 77 75
77 78 79 81 83
84 85 86 87 89 90 92 94
57 56 53 52 51 48 47 46
46 49 52 54 55 57 58
40 37 34 33 32 31
47 50 52 53 55
50 48 47 45 43 40
63 61 59 58 57 55 52
23 21 18 15 13 11
67 64 61 59 57 55
54 52 51 50 49 47
37 40 43 46 47 48 49
11 10 9 7 4 3 2
79 80 83 86 87 88
55 54 53 52 49 48
18 16 13 10 7
73 72 70 69 68 67 66 65
40 38 36 35 33 31 29 28
44 45 47 49 51
70 73 75 78 79 80 81 84
21 23 26 29 31 34 35
1 3 5 8 10 11 13
39 40 41 42 43 45 46
72 75 77 78 80 82
42 43 45 47 49 52
22 19 18 16 13 11
43 45 47 49 50 52 54
68 66 64 63 60 57 56
53 55 57 59 62 65
30 27 26 23 22
30 32 35 37 38 40
27 24 23 21 20 18 17 14
38 39 40 43 44 45 47 49
2 5 7 8 11
94 91 90 87 85 84 83 82
70 68 66 63 61 58
49 52 55 56 59 60
32 31 29 27 25 24 23 20
51 50 49 48 46 43 40 39
19 20 21 23 24
86 84 82 79 77 75
30 28 26 23 21 20 18
93 92 89 86 84 83 80 78
42 44 46 49 52 54 57
33 34 37 39 42
76 78 81 82 85 86 87 90
97 96 93 90 89 87 85
87 90 91 94 97 99
37 34 33 31 29
45 42 39 37 35 34
37 40 41 44 45 46 49
4 6 7 8 10 11 14 17
89 87 85 84 81 79 77
74 75 76 79 82 84 85 86
97 96 93 91 90 87 84 81
91 92 93 96 98
13 12 11 9 6 5
37 38 39 42 43 45
26 25 24 21 20
40 41 42 43 45 46 47 49
38 36 33 31 30 29 27
84 81 80 79 77 75 72
84 82 81 80 79
47 49 52 53 55 58 59 62
71 72 74 75 76 78 80
56 53 51 49 46 44 42
67 66 65 64 63 62 59
64 67 68 71 72 73 74
54 56 57 60 63 64 65
97 96 94 92 90 87 85 84
31 28 25 24 23
90 87 84 82 81 80 79 77
87 89 92 94 96
84 83 82 79 77 74 71
56 59 61 63 64 67
66 68 69 72 75 78 81
56 59 60 61 64
85 86 88 91 93 94 97
82 81 80 77 76
1 3 4 5 7 9 12 13
89 87 84 81 78
89 87 86 85 84 81 78 76
79 81 82 83 84
34 33 31 28 27
24 23 22 20 18 16 14 12
32 31 30 28 25 23 20
63 61 58 57 56 53 50 47
42 43 45 46 47 49 50
46 47 48 49 50 53 55
18 15 13 10 7
11 12 13 14 16 18 19
64 66 67 69 72 75 76
31 30 27 26 25 23 20 17
76 74 72 69 68 66 64 61
23 21 19 18 15 13 11 10
30 29 28 26 25 22 20 18
10 11 13 15 17 18 21
4 5 7 10 12 14 17
71 70 68 67 64
89 86 85 84 83 81 80
86 83 82 81 78 75
53 55 56 59 62
29 30 33 36 39 42 45 47
14 17 19 20 21 23 26
69 71 72 73 75 76
84 81 80 79 78
11 8 6 4 3 1
76 77 78 81 82 85 86 87
85 88 91 92 93 94 96
57 58 59 61 63
43 42 41 40 37
21 19 17 14 13 10 9 6
19 18 15 14 11 9 8
18 15 13 11 10
45 48 50 51 54 57
51 48 45 44 41 40
48 47 45 44 42 40 37 35
58 55 52 50 48 45
65 68 69 72 73 74 75
52 55 56 58 59 62 63 64
29 30 32 35 36 39
70 72 73 76 78
85 86 87 88 90
20 23 25 28 29 30
11 10 7 6 4
84 81 78 77 75
85 82 81 80 78 77
82 79 77 76 73 71 69 67
23 21 19 18 15 14 11
83 80 79 76 75 73
4 7 8 10 11
26 24 21 20 17 15 14 12
86 85 83 81 79 77
75 74 73 70 68 66 63
32 31 30 28 26
67 64 63 62 59
50 51 54 56 57 58 60 63
63 60 59 58 56 55 54
71 69 66 65 64 62 61 59
50 47 45 43 42 39
25 26 28 30 32 33 34
55 56 59 60 61
3 6 9 11 12 13 16
85 82 79 76 75 73 72 70
75 76 79 81 84
67 68 71 73 75 76 78
77 75 73 70 68
47 45 42 41 40 39 36
60 57 56 54 52 51 49 46
56 54 52 51 48 45 44
45 44 43 42 39
75 77 80 83 86 89 92
64 65 66 67 70 71
35 33 31 29 28 26
41 42 44 47 50 52 54 55
13 11 10 9 7 5 3
11 14 15 17 20 21 23 25
61 63 66 69 71 74
79 78 75 74 72 71 69
85 83 81 78 76
81 79 78 76 73
74 77 79 81 83
10 11 12 13 16 18 19
64 61 60 57 54 51
70 68 65 63 60 58 55
78 79 81 83 85 88 90
13 15 18 20 22 25 28
71 68 67 66 63
56 58 61 62 65
67 65 64 62 59 57 54
44 41 39 38 37
48 45 44 42 40 37 36 33
55 58 59 60 62
50 47 45 43 40 38 37 35
61 64 66 67 68 71 72 75
36 38 40 41 44 45
52 54 57 59 60
61 62 64 65 66 68
84 85 87 90 92 94 96 97
29 30 32 34 35 36 37 38
73 71 68 66 63 61 60 58
52 49 47 44 43 41
91 89 87 84 83 81 78 76
76 74 73 71 68 65 62 61
79 77 74 72 71 69 68 65
35 34 32 29 27
22 21 19 17 16 14
34 32 30 29 27 24
87 89 90 92 93 94
30 31 32 35 38
25 22 21 20 19 18
40 43 45 48 51 52
69 70 72 75 77
38 40 41 42 45 48 50 52
79 82 83 86 87
48 47 46 44 43 42 40
65 63 61 60 57 54
13 16 18 21 23 24 25 26
40 41 43 44 47 50 51
45 42 41 40 38
8 6 5 3 2 1
81 80 78 75 73
85 84 83 82 79 76 75 74
38 40 42 44 45 48
7 8 9 11 12
49 50 51 52 53
63 60 57 56 54 53 51
19 21 24 27 30
49 46 44 41 38
57 60 63 64 66 68 71
28 30 31 34 35 37
65 62 59 57 55 54
88 85 82 80 78 77 75
80 81 84 86 88 91
42 43 46 47 50 52 55
47 46 45 44 43 41
83 86 89 90 92 95
43 46 48 49 50 51
48 45 42 41 39
87 90 92 94 95
39 40 42 45 47 49 50
44 41 39 36 33 32 31 29
32 31 30 27 25 23 20 19
39 37 34 31 29
51 52 53 54 55 58 59
52 50 49 47 45
49 51 54 55 57 60 62 64
29 28 25 23 21 20 18 17
38 37 34 31 28 26 25
79 76 74 72 71 69
36 38 40 42 43 45 47 50
86 83 80 78 77 74
33 35 37 38 41 42
28 31 34 35 37 39 42 43
32 34 37 40 42 44 46 47
57 60 62 63 65 66 68
83 85 88 89 90
44 47 50 53 55 56 59 60
33 31 29 27 24 23 21 18
28 25 24 22 20 17 15 14
33 32 30 28 27
72 69 68 65 64
13 16 19 20 22 25
31 29 28 27 24 21
69 68 65 63 62 59 57
45 48 51 52 55 57 59 60
13 14 15 16 17 20 22 25
43 41 38 37 35 33 31
73 72 69 68 65 62 59
75 77 79 82 85 88
10 13 16 18 19 21
74 71 70 67 66 64 62
54 53 52 51 49
94 93 90 88 87 85 83
39 41 42 44 47 48 50
1 4 5 7 9 10
71 70 67 66 63 61
14 15 16 17 19 20 22
5 6 7 9 12 14 15
67 64 62 60 57 56 55
24 25 27 30 33 35
35 36 38 40 41
67 64 61 58 56 55 54
73 72 71 70 69 68
96 93 92 89 87 85 82 81
80 81 83 86 88
12 9 6 5 2
92 91 88 85 82 80
92 91 88 86 85 83 82 79
73 74 77 80 81 82
75 76 77 80 82 83 84
12 9 6 5 4
80 83 85 87 89 92 95 96
21 24 25 28 31 34
51 49 46 43 42
7 6 3 2 1
35 36 38 40 41 43 44
92 91 90 89 86 85
51 50 47 44 41
70 71 74 75 78 79
21 24 26 28 30 31 33
33 36 37 40 41 42 44
68 66 63 62 59 56 54
51 52 54 57 60 62 64
70 73 76 78 81 84 85
65 68 70 73 76 77
3 5 7 8 9 11 13 14
28 26 23 20 18 15
39 36 33 32 29 27
22 24 26 28 30
54 51 48 46 43
36 34 31 30 28 26 25 23
69 71 73 76 79
28 31 34 36 39 42 45
24 23 22 21 19 16 14
99 97 95 94 93 91 88 86
65 62 61 60 58 56 54 53
17 19 20 22 25 28 31 32
5 7 8 9 10 13 14
26 23 21 20 17
49 52 54 55 56
34 37 40 42 44 45 48 50
11 14 17 18 19 20 22 24
17 14 13 12 9 8 6
81 79 78 76 74 73 72
71 69 66 65 64 61 59
77 80 81 84 85
5 8 10 11 14 17
27 24 23 22 19 17
68 67 64 62 59 56
23 25 28 29 30 32
92 89 88 86 83 82
62 64 65 68 69 72
25 23 21 18 16
69 70 71 74 77 79
72 71 69 66 64 62
10 8 5 4 3 1
35 38 41 43 46 47 49 50
83 82 79 77 75 72
41 38 35 34 31 28 27 24
60 61 63 66 68 69 72 74
54 57 59 61 62 63 66 67
78 81 82 85 86 89 92 95
16 13 10 9 8 6 4
65 67 69 70 73
48 50 53 56 58 59 61
12 9 6 4 3
8 9 12 13 14 15 17 18
14 16 19 22 23
42 39 36 35 32 29 28 27
17 14 11 9 8 7 4 2
60 58 57 55 53
43 41 40 39 36 33
48 47 46 43 40
26 23 22 19 18 16 14 11
41 40 37 35 33 30 27 25
40 43 44 46 47 48 50 51
53 56 59 61 62 64 67 68
39 42 43 44 47 50 53 56
7 10 11 12 14
49 47 45 43 41 39 36 33
76 74 73 70 67
49 51 54 57 58 60 62
44 42 39 37 34
57 54 52 50 48 45 43
71 69 68 65 64 62
77 76 73 71 68 67
9 11 14 17 18
29 31 34 36 38 41 43 45
3 6 9 12 13 16
94 92 91 89 88 87
8 10 13 16 18
13 16 17 18 20 22 24 25
56 58 59 60 62 64
52 55 58 60 61
69 70 73 76 77 80 82 83
66 64 63 60 59
84 86 88 89 92 94 95 98
32 31 28 25 23
23 24 26 27 30 32
90 89 88 86 83
77 80 83 85 87 90 91 94
69 68 66 65 63 61 59 58
17 19 22 24 27
37 35 32 29 27 25 22 21
25 27 29 32 35 36 38
62 60 58 56 54
6 9 11 12 15 17 20
80 83 84 86 87
89 91 94 95 97 98 99
20 23 25 28 31 32 34
86 88 90 91 93
70 67 64 61 59 58 56 55
17 20 22 24 25
18 20 21 23 24 25 28 31
98 95 93 92 89 86
46 45 42 41 38 36
10 12 14 15 17 20
24 21 18 17 15 14 13
8 10 13 14 17 20
90 91 93 95 98 99
39 38 36 33 31
52 49 46 45 44 41 39 36
64 62 60 59 58 56 53 52
64 63 62 61 58 57 56
90 88 86 84 82
21 24 26 29 30
92 89 87 86 85 83 81 78
85 83 80 77 76 75 72
29 32 35 37 40 42 45
7 9 10 13 15 17 18 19
24 27 28 29 32 33
59 58 55 53 50
93 90 87 85 83 80 78 77
19 16 14 13 11 8
99 96 93 92 89 87
7 8 10 12 15
21 24 27 28 29 30 32
37 39 40 41 43
43 42 41 38 37
72 71 70 69 66 64 62
30 31 32 35 38 39 42
13 15 16 19 20 21
21 18 16 15 13
22 25 26 27 29 31 34 37
41 39 36 35 32
50 47 46 44 42 39 38 36
79 76 74 72 71 70 67 65
29 30 33 36 38 41 42
73 74 76 78 80 83 85 86
51 50 49 48 46 43
80 79 77 74 73 70 68 66
88 87 85 82 80 78 77
45 46 47 50 51 54 56 57
4 6 8 9 10 13
1 3 6 9 12 15 18 19
37 40 43 45 47 50
78 79 80 83 86
61 63 64 65 66 69 71
22 25 28 31 34 37
24 26 27 29 32
91 88 87 84 83 81
78 81 82 83 84 86 89 92
17 18 20 23 26
34 31 29 26 23 22 19
20 19 16 15 13 10
32 33 36 39 42 43 46 48
78 79 82 83 84 86 87 89
67 69 72 73 75 78 81 84
62 59 56 53 50
91 90 87 85 82
51 53 54 55 58 61 63
67 66 64 62 61 60 57
80 77 74 72 69 67 64 61
59 62 64 65 67 70 71 72
45 43 40 38 37 35 32 29
85 82 80 78 77 76 74 72
71 69 66 64 61 59
7 9 11 12 14
73 70 69 66 63 62 61
75 78 81 84 86
51 50 49 46 44 41 38
97 94 93 91 89 87 84
8 5 3 2 1
21 19 18 17 15
96 95 94 92 90 87
56 58 59 62 65 67
49 52 53 55 56
43 42 40 37 36 33
57 59 60 61 63 66 68
59 62 63 66 67
75 72 70 67 65 64
73 70 69 67 64 62 59
77 74 73 72 69 67 64
46 47 49 51 53
30 31 32 35 36
23 25 28 30 32 34
82 84 86 89 90 92
75 73 72 71 68 67 66
64 62 61 58 55
97 96 93 90 89 87 86
20 21 23 24 27
59 58 57 56 54
64 63 60 58 57 55 53 52
74 72 69 67 64 62 60
23 26 29 30 33
30 31 33 34 37 40
87 86 83 81 79 76
53 55 57 59 61
26 28 31 32 34 36 37 38
36 35 34 31 29 26
19 16 14 11 8 5
41 40 38 36 34 33 32 30
55 58 60 63 64 65 66
52 51 50 48 46 43 41
41 38 36 33 30
83 80 79 76 74 71 68
45 44 42 41 38 36 33
21 20 18 17 14 11 10
3 6 8 11 13 14 17 18
81 78 75 73 70 68 67
15 16 19 21 24 27 29 32
78 79 82 85 86
35 37 38 40 42
72 71 68 66 65 62
73 72 70 68 67 64
40 43 45 46 49 52 53
76 75 74 71 69 66
68 70 71 72 75
45 43 40 37 35 33 30 29
8 9 11 13 14
78 75 73 71 69 67 65 62
40 37 34 32 30 27
79 77 74 71 69
74 75 76 77 78 79 81 84
44 41 38 36 33 31 28 25
78 75 72 70 69
79 82 83 86 87 90 91 94
35 32 30 28 25 22
96 95 93 90 88 85
85 88 90 92 93 95 97
58 55 54 51 49
22 19 18 15 12 9 7 4
92 90 87 86 83 82 81
86 85 84 83 81 79 78
67 65 63 62 61
80 79 76 74 71 70
14 17 20 21 22 24 27 28
97 95 92 89 87
23 26 27 30 32 33 35
58 57 55 52 50 47
67 70 72 75 76 78 81
10 12 13 16 18 20 23 24
65 66 67 70 72
10 7 5 4 2
35 34 32 31 30 27 25
38 40 43 44 46 47
77 80 81 83 84 87 89 92
80 77 76 75 74
30 31 34 35 38 41 42
86 87 88 91 93
77 75 74 71 70 68 66 63
25 26 29 32 34 37
33 31 30 29 28 27 26
15 17 20 23 26 28 30
86 83 81 78 75 73 72 70
73 70 67 66 65 63
19 18 17 16 13 11 9 8
35 38 39 41 42 45
24 22 20 19 16 15
5 6 9 11 13 14
31 34 37 40 41 44
75 76 78 80 81
52 49 47 45 42 41 38 35
80 77 74 72 69 68 67 65
50 47 46 43 40 38 36 35
50 49 46 43 40
83 82 79 76 75 74
82 85 88 89 91
22 24 27 28 31 32 33
95 92 90 89 86
19 21 22 25 28 29 31
15 17 18 19 21 23
75 78 79 80 82
80 78 75 74 72 71
51 49 47 46 43 42 41
55 56 59 60 63 64 66 67
67 66 65 63 61 58 56
42 43 45 48 50 51
29 32 35 36 37
5 8 9 12 13 14 17
81 84 85 86 87 88 90
59 60 63 65 68 71
43 41 38 35 33 30
29 31 33 34 36 39 42 43
72 69 66 63 62 60
13 12 10 7 6 5 3 1
48 50 53 55 57 60
39 41 42 43 45
80 78 75 72 69 67 64 62
82 80 79 76 75 73 71
46 43 40 39 38
55 52 51 49 48
81 78 75 72 71 70 69 68
38 37 35 33 30 29
36 38 39 42 44 46 49
73 72 70 68 66 64
10 9 6 5 4 2
7 10 13 16 17 18
74 72 69 67 65 62 61 58
44 47 48 50 52 54 56
75 77 80 82 84 85 88
14 16 18 20 23 24
65 62 61 58 56 54 52
22 19 17 14 13 10 7 6
53 50 48 46 43 41
10 12 14 17 20
24 21 20 18 17 14 12
66 65 63 62 60 58
84 83 81 79 76 73
37 38 41 43 46 47 50
88 86 85 82 80 78
5 6 9 10 12
64 62 61 58 55 53 52
29 30 33 36 39 42 45
89 90 91 92 94 96
41 39 37 35 32 31 29 28
83 85 86 89 92 95 96 99
84 86 88 90 91 94 95 98
77 79 82 84 87 89 91
19 21 22 23 25 26 28
20 23 25 28 31 34
84 81 78 75 73 70 67 66
11 8 6 4 3
64 63 61 59 56
88 89 92 94 95 97 98 99
87 84 83 80 77
66 68 69 70 71 72 75
19 22 24 27 30
31 33 35 38 40 43
60 63 64 65 66 69 71
70 73 75 77 79 80 82
21 20 19 16 15 14 12
33 30 29 28 27 24 23 20
30 31 33 35 36 38
19 17 14 12 11
76 77 79 80 83 84
29 30 33 36 39 40 42
24 21 18 17 16 13 10
32 34 35 38 39 41 42 43
11 10 7 6 4 2 1
2 3 6 9 10
58 55 53 51 50 47 44
49 46 43 40 37 35
56 57 59 61 62
80 82 85 88 91 93 94 95
89 86 85 82 79 76 75
57 55 52 49 46 43 42 39
40 41 44 45 46 48
12 9 6 3 2
74 76 78 81 84 85 86 88
89 91 92 93 94 97
76 74 72 71 69 68
42 43 44 47 48 50 53
42 43 44 47 48
86 88 90 92 95
15 18 20 22 24
74 71 69 68 65 62 61 59
5 8 10 12 13 16
8 10 13 14 17
32 29 28 26 24 22 19 18
99 96 95 92 89 87
48 49 50 51 54
38 39 41 44 47 48
82 84 85 88 89 91
39 41 42 43 46
83 80 79 77 74 73 72
85 84 82 79 76 75
49 48 45 43 40 38 36
9 12 15 18 20 22 25 27"""
