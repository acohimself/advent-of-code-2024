{-

   Andreas Christian Olsen
   aco@acohimself.com

   https://adventofcode.com/2024/day/3

-}


module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h2, i, input, label, text, textarea)
import Html.Attributes exposing (action, class, for, placeholder, required, rows, style, type_)
import Html.Events exposing (onClick, onInput)
import Maybe exposing (Maybe, withDefault)

title : String
title = "Day 3: Mull It Over"

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


part1 : String -> Int
part1 input =
    0

part2 : String -> Int
part2 input =
    0

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
inputString = """-:-]what()(+/mul(957,396)?mul(550,844)%+why())-? #}from()mul(488,628)%} ~**mul(770,931)$~mul(791,733)<{mul(985,350)<#why()don't()what()select()$what())]what()who()mul(327,185))<^^mul(542,68)#?who()<from()';^how()mul(619,952)/where(){(!);'@,mul(551,161)select()>when()do()from()mul(51,291)[where()!{]/}'@?mul(233,511)@what()]mul(311,967))&who()how()mul(839,578)^who()]}mul(266,735){mul(176,670)mul(154,710)*select()](':^,mul(531,801)# *why()why()mul(30,325)~,where();select()select()}-/when()mul(512,729)+where();[mul(720,339)[~*when()mul(722,867)!);{+mul(582,286)^:)what()@mul(604,485) (who()why()who()from()[mul(128,295)how()?!%~~<what()mul(156,267)'how(689,161):where()mul(206,221) mul(835,81);] %mul(562,798)^{%where()mul(38,166)#!what()mul(185,550)^! ?,+;}}mul(685,101)select()mul(316,869)>[~}{[&;mul(548,186))(%];mul(841,290)when()where()'?mul(646,803)mul(553,782)how()when()-mul(569,604)@ ++:/%why()]select()mul(257,598)#mul(897,819)how()what()from()how()]when()~/!}mul(856,271)+&why(){}why()>who()mul(373,408)-who()^&%>';,when()mul(54,88)what()!mul(663,711)$#(?;^from()mul(898,810)when()from()%@mul(776,102)why()mul(303,842)!/,%<^;mul(840,791){[@mul(909,714)don't()>who()why()from(545,686)%~mul(483,956)^'why()from()where()$>*:mul(931,649) mul(800,313)):mul(31,69)mul(549,670)$;mul(327,976)@who()^mul(627,907)/[@what(925,706) ;^who()!mul(629,813)when(){$&where(){#mul(504,147)?mul(222,429)#who()*/!select()<what()mul(310,934)])^/$]mul(917,911)-%how()?[;,mul(823,273)*[~select()/select()~%mul(703,815)what()where()?what():'?[who()mul(966,52)how()&-~(%[(^mul(913,694)mul(190,570),&<%'from()where()what(979,309):+mul(662,926)/<!when()'select(),/&*mul(655,410) how()}mul(386,934)where()[mul(159,595)@']from()when()#?@mul(356,559)&)>?$ mul(439,336)?from()*select()*}when()]who()mul(148,356)(who()+mul(271,905)mul(564,172)@?(~mul(628,470)#} mul(301,170)?@,^don't();&:^&how(605,264))(what()'mul(896,108),$mul(413,431):<>where();how()>!}when()do()!}/&:when()>mul(754,737)~:#:* mul(962,620)}mul(26,490)!~,mul(450,234){/]who() <;{mul(938,932)?--#%};}mul(140,314)(-}/,[mul(202,839)^why()-!},mul(459,716),mul(600,932)}when()]who()}who())&mul(752,142$mul(174,573)mul(231,182)from()mul(308,661)what()} mul(178,99)&^*[(mul(858,730)~}from()^mul(17,545)what()~{~*^what(90,714)how(),mul(63,969)-]mul(364,49);mul(130,598<:&why()select(879,794)mul(46,169)>&how()%mul(564,506)/what(255,868)mul(415,375)':@who()mul(525,772)<$mul(277,787)[~mul(314,321)'&@$^%mul(741,325)?(%#;*mul(459,703)why()~what()mul(250,38)what()<-$)when()%(don't()mul(296 ',mul(673,933)?#<@who()from()^?mul(831,912)?/*mul(407,853)mul(119what()why()>mul(816,762) how()<}~from()how()/@:mul(792,799)()@?%:$:mul(289,264)
why(),'*),from()mul(767,350):>~mul(123,684)@from()>mul(722,338)<'<select()what()}*select()mul(993,866)where()[;'select()what()select()?why()mul(357,705)what(){+&mul(590,928)how()[why()how()+what()><why()how()mul(417,421)/[?where()mul(298,609)*%who()#where()}why()(~+mul(233,280)when()mul(740,13)where()#why()!/mul(702,342)@;mul(945,10)[}!#mul(758<mul(969,595)+how()from(573,280)what()mul(351,944)what()where()]+<how()&[^mul(211,333)what(590,707)<'mul(579,209)do()^what(324,278):<+)!mul(482,641,^why()/<$from()select()mul(302,432);$where(){*mul(58,633)#]+,select()'$mul(534,736)@+~@what()/mul(492,990)'mul(606,447)~}, #@$)$/mul(878,726)'++'who()!where()what(777,25)why()mul]@>mul(252,529)do()%)%^when()!mul(731,123)mul(582,378)-mul(934,355}where()what()how()@%<[>;<do()(mul(709,553)])?&~mul(936,634)when()who(),do()#$$mul(917,683)select()when()+mul(858,234)!mul(828,850)@(?)^)mul(244,230)*^/%what()[^$who()@mul(716,56)from()/mul(312,20)!)mul(333,734)select()from()from():$how()+$${mul(443,208) &when()&$-&:mul(4,320):*;&#~~mul(261,16)&select()'@who()~^+mul(597,905)]]()who()(mul(273,2)who()how()#+?when(263,200)/mul(699,563)$don't()why()]!mul(744,844)>:mul(5,63) who()!mul(946,479)+}select()$@mul(478,71)~<()-#mul(500,398)'from(612,869)select(98,846)!!<]:select()mul(785,98)',select()//:-mul(192,182):{ why(){;mul(910,471)&^+&who(424,579)/:select()[mul(217,829)where()how(423,674)$*mul(318,444)[!?~mul(254,793)who()select(521,861)how()mul(54,186)why()[:^;-mul(109,234)mul(465,266)+?([-mul(518,485)^mul(403,383)mul(726:how()when()#mul(344,785)/}+?mul(97,996)+@'mul(637,749)how()-%+mul(302,591)don't()%^]}!)]mul(524,902)$[]/*mul(839,651)}^)$when()'what()mul(270,805)#mul(566,750){ how()/mul(716,302)/&*>@-what()>when()what()mul(602,574)+where(619,83)mul(235,585)?where()[%select()+>where(){]mul(209,404)~where(),why()how()-/-mul(123,125)^'how()who()$}/;mul(851,629)how()who()'#;$ do():select()!when()[>+>mul(576,669)what()!,'?from()&mul(423,260) how()-mul(910,922)?<@'->(mul(274,173)why()}<~from(601,667)mul(282,816)<how()when()when()' ^from()mul(939,359)mul(385,79)who()&when()-^?-^don't()how()+*/mul~@, @/!$mul(526,30)}(:~from()do()who()$/!~)what():%mul(366,255)(*+>from(953,125)~mul(98,837)&[how()[what()mul^%} '$from()^-;mul(439,801)mul(761,498))>$'-why()<</mul(557,979)[mul(224,700)*mul(917,173)[-(~;mul(44,236)%select()what()( ![(who()mul(153,275?how()(!/mul(627,465)%/>who():mul(109,723)from()who()mul(391,416)& -how()>mul(496,170)^'${mul(657,96)why(611,959)why())from()%+mul(62,175)>}~,%from() >mul(92,927)mul(17,279)!why(){+where()}how()mul(939,286){;mul(744,904) from()?''who()&who()how()mul(170,192)@,mul(499,985)';from()})!{+who(){mul(572,653)#:why()mul(720,626who()from()(%:mul(676,779)!why() when(58,261)^ *mul +from(){)')@!*mul(802,646)how()(-+[;#when() {mul(742,576)<where()>]* ;mul(38,717)?what()}'how(184,130)mul(126,115)]@>mul(512,978)/where()+*why()mul(705,242)?{$how();when()mul(173,898)'(~who()mul(536,621)&^from()select()when()?@mul(52,680)] -%@;][mul(971,573)^mul(879,466)
mul[!-,why()(?mul(770,624)~:mul(197,580){{(%mul(678,337)what()mul(951,457)'<#mul(679,253)?do()who();&  who()select()mul(105,231)+{&!when()who()how()!')mul(262,609)):,{'from()/?@}don't()*:mul(471,462):([(@;from()mul(734,965)/&/where()mul(885,324)why(){>/#<:mul(390,818)!mul(684,730)*>>{[!what()what()mul(824,893)mul(801,597/why()!when()how() '>from()%mul(165,825)who(837,795)?? why(187,949)mul(703,551) when()'mul(759,898)select()#$$  where()where()mul(694,292)%(who()%#}mul(884,767)^<mul(900,512)^what(862,782)%*}mul(484,385)]where()where()mul(593@{what()?{{@&who()^*mul(511,975)+%from(193,630)@-~how()/<mul(161,30)mul(462,217)mul(283,656)when()mul(733,613)what()^what()why()/where()what()^!mul(777,858)^select()mul(732,203)how()<mul(574,457)select()where()$who()when()~where()#}mul(525,448)*mul(5,230)mul(347,280)<from()#what()mul(613,165)@who(609,769)@~/,%#;*mul(298,242)what()+~when():($-(mul(118,981)mul(72,529)$^~select(306,809),!/(mul(864,257))what()-?mul(308,631)? //*&,/!don't())>:$mul(945,214) where()how(837,748)(mul(537,489)(%{mul(594,245)who()from()from()%}!)])why()mul(832,704)^~mul(902,18)how()what()who()/#'-#mul(345}who()when()?]&^mul(873,161)^mul(81,518)]$mul(875,700)how()where()(mul(735,369)/-< why()@>mul(703,274)-<mul(949,449)what()+(>from()when()**,)mul(204,896)]+mul(28,596);why()#select()'#]mul(774,171< why() !from()mul(29,711)mul(28,600)}when()!@(^why()~{mul(555,761)%}do()from()-mul(139,179){how()'?mul(294,354)}where()mul(317,957)]select()#;where() where()^@from()mul(461,831)~who(75,120)%who()@-&;,)mul(563,131)mul(607,401)select())from())&,mul(26,412)select(726,797)&)how()@select()mul(416,615)(what()select()!()<<what()who(299,911)mul(628,777){mul(712,971)(*]what()from()<>#;&mul(921,158)>- mul(245,598)select()what()*({mul(468,522)]^select(574,638)]@};do()from(799,930)%who()#:[how()mul(413,674)[how()*mul(327,432)%?@'mul(486,644)}~>-mul(637,16)how(254,818){%where()#where()who():who()mul(431,749)$$where(228,303)<!why(109,819) ]mul(693,21) where(396,89)[mul(60,538){mul(738,595))-&why()how()why()(mul(698,770))*>,%+&mul(593,337)}#:where(){why()[+&!mul(520,180)^mul(723,959)mul(492,739){/?mul(327,463)?-select()-<,mul(102,950)!#do()@when()-when()},select()/mul(274,979/from())who()(~)!^$mul(78,739)why()@{~mul(819~?~+]>@mul(824,208)}mul(338,41)where() &*;;&what()mul(888,646)from(),$:-who(263,596)!!-from()mul(396,204)>{)>{?(%{mul(885,937);!what()&%!<mul(178,507)mul(833,974)^mul(842,332)>from()}mul(207,417)(<;what()mul(187,893)((&&}&why()*)-mul(589,985)^how(310,10)when(),]:mul(989,534)$where()mul(552,604;(!;select(){){where()#mul(152,511)+/$!;+/<+]mul@select()::when()who()how()!what()mul(446,994)from()<}what()select()'why(244,102)+<select()mul(516,369)]&mul(707,483):@}%?what()from()don't()mul(407,712)$:{&:,,,?mul(112,3)-~- what()>)%}who()mul(693,903)?*/}select()mul(282,115)[];why(){mul(925,276)how()&{what()&$when()&-!mul(191,385)*)mul(446,703)[>what()?*}what()^who()*mul(997,718)!>$)@,^*)mul(788,724)from()#&]~/<,)how()mul(181,531)[(why()[mul(409,206)what(604,231) when()why(921,229)]>where()//mul(199,96)when()why()#!/usr/bin/perl~%>~ &:mul(239,884)
what())[$^+why()mul(118,89)mul(689,203)#,,what()what()mul(633,394)from();~from()!where()*?@mul(568,753)/[+when()?%:mul(828,475)what()don't()-;@ mul(509,544)[when()};<+'(mul(235,757)<+:%select():mul(241,373)$mulselect() (%where()where()]-)mul(306,62)from()what()mul(285,414)!*&&>who()#mul(543,866)/~select()[+don't()/><why()from() ~#mul(983,515)#mul(731,258)mul(894,831){>)where()!when(),'mul(701,189)/';!:]+how(902,507)-^mul(847,104)mul(883,112){'~}&mul(697,190)what()%![!<where()@why(319,27)select()mul(468,827)/mul(914,407@ where(661,684)mul(850,467);don't(){what()##*%mul(175,88)>(why()@^where()who()mul(843,4)%what()&-%mul(945,389)#<)select()mul(690,755):mul(774,780)}mul(940,623)%where() ?where()(}^!mul(34,627)mul(479,281)#}>what(157,964)select(){how()why()<don't()(*>+who(640,716),mul(117,249)mul(395,14)-]( ~(how()-mul(220,860)+^mul(765,74)select(),$who()<mul(685,855)!what()where()why()#{,-;mul(302+^(who()]from()~<?where():mul(383,623)::~(>,@mul(639,551);(mul(26,80)why(480,988)^why():? <[mul(672,116@mul(316,879)}mul(498,582)  @,:{'why(589,887)mul(39;]mul(855,31)^from()?mul(93,510)mulwho(){@-what()<where()~select()mul(225,679) 'mul(305,662)/:,mul(887,727)?:~:&/select()#:<mul(755,830)^#}+@)how()?^mul(14,79)-mul(433,926)what()'mul(458,711)>from();where()from()~:(mul(688,799)~when()>from()-select()who()~<mul(71,560)( who()? +#^who()[mul(70,477)}>#+{ >mul(275,916)#?-mul(359,865)where(441,672)'}what()[from()when()<>who()mul(909,227)$:!'mul(330,478)!?[;(+#what(919,466)&/mul(73,181)[{how()mul(403,182);mul(249,968):!from()?!{#-mul(661,975)!-;}'mul(999,285)select(565,140)+>@,$:from(47,825)~!mul(599,436))<when(603,518)@mul(567,365)?(}}where()<{:who()?mul(755,613) <*who()mul(448,608)$$mul(507,887)mulwhen()]when()^$+@();+mul(846,707)]mul(932,511)who()}}where()#mul(122,316)#>+ [>%!mul(959,617)mul(5,150)where()@;&mul(630,775)<select(686,710)who()/';[*%select()mul(859,837)'mul(451,3)(when()($/+mul(499,794){]mul(194,78)from()'?don't();mul(47,314){who()~/]mul(526,464)why(666,133)#}/;]:mul(223,721)how()><&-:;mul(114,820[don't()when()#how()mul(976,290;+-from()where()]mul(338,47)when()what()where()%]mul*who() #+what()mul(679,324)mul(859,321)%'mul(446,297),#select();<mul(797,938)who()mul(531,845)}*select()@{mul(661,677)/)'*mul(837,800)<who()select()<]}<@mul(844,594))when()where():'mul(75,307)mul(676,67)(]what()<select()?mul(851,92)+&)mul(683,41)?<,/:~'mul(179where()from()~<mul(674,897)/!how(529,298) *[}from(),mul(362,131)}! % $mul(168,955)mul(517,218)[(,mul(740,567)}~[)$@from(),do();]mul%/%]why()mul(777,920)what()what()$;<what()mul(307,462)-+from()what()/$select()select()do()<select()&from()why()why()&mul(522,562))<how()-!+when(413,672)&)mul(621~(?select()<*-why()mul(372,852)}]~mul(668,335)mul(512,514){%mul(873,547)[*(mul(887,814)]>';from()mul(206,301)why()-'mul(87,734)<where()+:mul(844,819){#^^mul(130,409)[('@what() ;mul(916,629)@@who()-?}mul(320,288)what()?mul(184,677)[@#select()+::-select(929,36)/mul(329,568)
{^mul(75)<:from(){select()select()mul(833,211):[-'mul(702when()who(21,237)+)&!>mul(735,118)] {/what()*@^what()where()mul(685,883)when()what()mul(91,84)}]<select()why(795,767)@why()]}$mul(711,291)select()--+<*who()mul(898,469^how()>-~where(){when(374,912)$do()>~,' how()mul(67,939),)&~@where()(mul(923,997)~,*$when()[from()from()mul(699,705)~/when()+}mul(214,775)'%mul(715,142)-;mul(290,853)from()$why()what()select()-@-mul(682,729)+^$}, !mul(197,39)mul(269,573)@^mul(819,945)/select()-do()%$~'#select() 'mul(64,956)[who()*who()@,)>+}mul(519,356);*/%>why()[[)mul(184,666)what()mul(892,160)+*&where(764,214)--]?(>mul(996,338)>when()what()]))&?mul(865,864))?@[::when()>what()>mul(218,296)&)%:<?mul(903,712),+mul(102,86)' ^,[>[~why()how(416,393)mul(118,100)from()select()#: $mul(7,742)how()&~how()>who()^mul(667,218)?/mul(218,898)!:$from()>mul(746,995)(+select()] when()}mul(971,429), !,&mul(505,165):!-)?-how()from()%mul(921,456)@mul(491,404)#how()-mul(284,834)#?(mul(667,960)]where():how()what(963,920)% what()don't())mul(236,336) why()why()select()where()^mul(33,901)mul(253,866)<#>$]%-mul(216,512),)from()what()what(464,183)where()select()@'}mul(98,640)' :)what()$%~#/mul(283,893)(mul(520,959)#^?{>@*when()when()]mul(80,420)where()mul(751,106)when()when()! how()mul(148,813)@><^[mul(740,246) how()where()!!/mul(242,262) who()who()%,mul(308(#/when()$mul(359,332)from()why()from()~$$^where()@mul(270,445)why()}%mul(623,449)where()how()?& />mul(759,849)-} (mul(84,200),$;who()[!mul(934,76)(@^:where()why()/'mul(650,46)[mul(105,265):)when()+ &'what()^mul(334,290)] $what()/mul(930,226,>^!%don't()mul(243,460){'#/what()+/mul(437,684)mul(501,579)/mul(243,174)mul(988( +<#where(206,306)where()[,from()mul(331,170)(?^~}#mul(940,949)>(,*+mul(925,961)^*from()select()^--^)(mul(994,182)+'+mul(153,816)~<!?[mul(31,276)don't()how():/!*] !^~mul(404,438)from()<]?)})>&mul(260,998)~:],],who()why(348,109)why()<don't()]#?)select()<<where(999,395)mul(342,366);what()select()~&~mul(120,353)when()]mul(198,41)][;(]:select()-[mul(947,937)$$when()-{mul(672,675);/!mul(14,647)>who()when(290,863)why(735,89)!from()@ >#mul(205,831)'$ %'mul(981,644)!<mul(100,956):{#select()#*mul(168,493)who()-%)#;{don't()^]who()~~ *mul(47,958)mul(130,976)how()~how()?what();mul(983who()(why()/)},]*:/mul(374,969)>#,*>mul(910,535))[who()#mul(220,934)how(271,592)-,*$,+mul(11,63)mul(783,580)^&*}~<mul(677,425))select()when()/how()mul(181,298)??why()how()&/mul(670,720)/@!when()where()#mul(403,306)]from()(from()why()*why()don't()^~from()[select()who()select()mul(516,37)what())}-*>(;from(){mul(21,444$what(274,202)<where(),}mul(706,78);select(275,134)]{&}why())@mul(302,87)[@[who()!when())mul(196,98)mul(918,675):,>how()mul(293,78)mul(189,814)#{}why(80,738)}from()mul(456,691)*@{(,[%mul(916,89)how())(]@mul(524,412)];:(mul(374,851)where()[mul(473,999)select()$what()}don't();];(}mul(327,69)%&+;:/what()don't()-%,'()mul(377,536)mul(31,322))@what()^select()mul(719,58)$why()~&what()']}mul(39,890)(+<#who(){how()!<mul(559,894);-&what()from()#$do()mul(256,3)where()^how()when()$:<what() mul(664^-from()}mul(469,922)from()>mul(299,824)(]](+mul(205,11)
{mul(602,646)!% ^mul(384,953)do()&&:]<<select()#<mul(766,701)&@(what()who()+>mul(228,736){({}don't()-when()@*>-mul(278,494))/&<mul(596,290)select()when()),{ ~mul(563,584)mul(351,221)how():#where()[{%what(128,820)[ mul(754,363)!{)how()&''from()mul(388,712)?;'>?><(select()mul(916,219)select(),:{(<*mul(360,209)why();~}where()-(,mul~when()when()!where()@:;:#/do()select()select()!,how(972,480)~-how()who()mul(726,867):>mul(549,954)](how(802,821)select()how()[+^mul(535#select()}why()[what()]why()*{#mul(72,525)mul(809,41who()!~where(82,230)}mul(961,563)}who()what(417,588)<how()mul(756,106)!)when() [mul(733,591)how()[why(),<+@,mul(889,603)when()when()mul(60,212)mul(955,930)]~&)/!}-}&mul(206,66)where(882,366)~/<{mul(745,261)'<where()<why();select()(how()mul(74,146)where():~who()what()>&*mul}{,+?%^[when(34,108)who()mul(71,111)$+,}#~-<mul(265,932)]$what();^mul(775,136)mul(192,60)how(){)mul(212,476<who()?mul(264,266)[,^&/how(215,234)mul(447,79)!!+;,who()@{mul(897,924)^what()-%((don't()mul(259,393)who()when(956,639)&where()]@{,mul(637,712)%(mul#'select()!~<+<do(),#&<:% -{)mul(914,330)when()-)?@]{where(240,411)*select()mul(191,984),%?-what()+{mul(623,7)+mul(899,145)</why()select()what()<{~mul(849,743)!where() mul(43,318)when()!~$)mul(607,51)!?<mul(123,500)<]how()!what(){(mul(380,536),mul(367,657)~{,why()?-*$mul(589,520)(#mul(568,853)!-how()'^what()}'^mul(924,205),};'/^!!@mul(765,625)*!+where()[@select()-~do()!!,who()+%mul(369,495)()/mul(530,282)mul(148,870)]*where()mul(668,764)%who()how()how()&![who()mul(678,727)>^]@ when(){:@mul(545,335) &how()from()<:mul(958,59)$ (what()mul(886,155)how()&+mul(525,132)<&}who(502,340)select()when()#^mul(339,398)who(),(how())@@how()$$mul(831,852)when()don't()>mul(935,45) select(930,598)!:select()mul(693,62)(mul(681,508)how()^@*;)mul(235,731)who(468,32)#~>don't()#:what()where()when()~<who()-/mul(137,827)!who()!why()mul(806,939)from()mul(492,899)why()^),who()]#from()$ mul(384,731)>~^>@#{mul(2,62)mul(866,617),>% mul(806,586)how():{/mul(669,407)!/mul(457,522)mul(40,474)>)]select()who()}mul(451,794)]#mul(115,471)~mul(331,456)@^who(){#when()mul(672,70)where()who())(#how()what()!mul(438,748)-/mul(605,224)mul(46,34)~?'</'$#how()&mul(278,426) mul(710,864)mul(296,109);mul(481,73)&<,]mul(650,979) mul(575,478)how()$@%#;mul(319,368)][select()where()/ /,:when()mul(399,630)from()mul(9,26)~'do()mul(953,316)%''%{>mul(650,523)?mul(871,731)?%from(966,980)>when()mul(790,779) from()#$:,'mul(428,763)@# why()mul(724,6)select() ,mul(208,11)why()how()mul(287,562)$[:select()},?mul(531,36)/mul(12,351)#$+,:#:$when():mul(941,380)-[from()!)?-select()mul(102,544)when();~{select()~^;-do() ]who()from(),<who()]/>mul(959,599)>why()/,<%-,!mul(517,558)%])$mul(945,968)mul(854,265)?<when()mul(98,913)<&&}]+mul(830,92)where()]why(132,949)why()from()when()'when();{mul(615,131)(,;}^~!mul(973,226)how()^}){}mul(45,238) where()#)'!~ <"""
