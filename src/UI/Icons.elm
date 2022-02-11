module UI.Icons exposing (..)

import Color.Dracula
import Element exposing (Color, Element, html)
import Hex
import Html.Attributes exposing (attribute)
import Svg exposing (Svg, g, path, svg)
import Svg.Attributes exposing (d, viewBox)


toElement : Color -> String -> List (Svg msg) -> Element msg
toElement color viewBoxValue svgImage =
    svg
        [ attribute "version" "1.1"
        , viewBox viewBoxValue
        , Svg.Attributes.fill <| printColor color
        , attribute "style" "filter: drop-shadow(0 0 2px black);"
        ]
        svgImage
        |> html


printColor : Color -> String
printColor color =
    Element.toRgb color
        |> (\c ->
                (Hex.toString <| round (c.red * 255))
                    ++ (Hex.toString <| round (c.green * 255))
                    ++ (Hex.toString <| round (c.blue * 255))
                    |> (++) "#"
           )


comedyMasks : Element msg
comedyMasks =
    toElement Color.Dracula.white
        "0, 0, 128, 128"
        [ g [ attribute "stroke" "none", attribute "transform" "translate(0.000000,128.000000) scale(0.100000,-0.100000)" ]
            [ path [ d "M470 1144 c-96 -32 -206 -40 -320 -25 -80 11 -94 11 -115 -3 -14 -9\n-27 -27 -31 -41 -11 -43 23 -337 46 -406 43 -127 139 -247 233 -289 50 -23\n124 -26 172 -6 89 37 180 178 211 326 14 65 14 97 4 233 -6 87 -16 171 -22\n188 -19 56 -60 61 -178 23z m54 -224 c18 -7 26 -17 26 -34 0 -30 -25 -46 -73\n-46 -59 0 -77 52 -27 77 30 15 41 16 74 3z m-258 -41 c19 -21 18 -21 -9 -45\n-30 -26 -52 -29 -91 -14 -52 19 -19 80 44 80 25 0 43 -7 56 -21z m247 -282\nc-58 -98 -248 -103 -298 -7 -23 45 -19 49 27 27 78 -39 222 -18 268 38 14 18\n15 17 18 -4 2 -12 -5 -36 -15 -54z" ]
                []
            , path [ d "M710 921 c0 -15 15 -24 61 -40 108 -36 206 -46 327 -32 106 12 107\n12 125 -11 17 -20 18 -34 13 -103 -19 -231 -42 -326 -105 -422 -90 -139 -217\n-196 -319 -144 -71 37 -138 131 -168 236 -10 36 -21 65 -24 65 -3 0 -11 -9\n-18 -20 -10 -16 -8 -31 12 -86 77 -211 236 -305 388 -228 109 55 209 197 242\n344 25 110 40 325 26 360 -23 57 -49 64 -172 49 -121 -14 -212 -5 -315 31 -70\n25 -73 25 -73 1z" ]
                []
            , path [ d "M743 673 c-20 -7 -15 -51 6 -63 39 -21 111 -5 111 24 0 34 -71 58\n-117 39z" ]
                []
            , path [ d "M1006 644 c-9 -8 -16 -18 -16 -22 1 -30 55 -54 97 -43 34 9 44 48 17\n68 -24 18 -79 16 -98 -3z" ]
                []
            , path [ d "M825 446 c-34 -15 -85 -70 -85 -91 0 -11 9 -9 35 7 68 39 193 27 252\n-25 l36 -32 -7 30 c-8 37 -33 72 -69 99 -33 25 -121 32 -162 12z" ]
                []
            ]
        ]


team : Element msg
team =
    toElement Color.Dracula.white "0, 0, 512, 512" <|
        [ path [ d "M239.208 343.937c-17.78 10.103-38.342 15.876-60.255 15.876-21.909 0-42.467-5.771-60.246-15.87C71.544 358.331 42.643 406 32 448h293.912c-10.639-42-39.537-89.683-86.704-104.063zM178.953 120.035c-58.479 0-105.886 47.394-105.886 105.858 0 58.464 47.407 105.857 105.886 105.857s105.886-47.394 105.886-105.857c0-58.464-47.408-105.858-105.886-105.858zm0 186.488c-33.671 0-62.445-22.513-73.997-50.523H252.95c-11.554 28.011-40.326 50.523-73.997 50.523z" ]
            []
        , g []
            [ path [ d "M322.602 384H480c-10.638-42-39.537-81.691-86.703-96.072-17.781 10.104-38.343 15.873-60.256 15.873-14.823 0-29.024-2.654-42.168-7.49-7.445 12.47-16.927 25.592-27.974 34.906C289.245 341.354 309.146 364 322.602 384zM306.545 200h100.493c-11.554 28-40.327 50.293-73.997 50.293-8.875 0-17.404-1.692-25.375-4.51a128.411 128.411 0 0 1-6.52 25.118c10.066 3.174 20.779 4.862 31.895 4.862 58.479 0 105.886-47.41 105.886-105.872 0-58.465-47.407-105.866-105.886-105.866-37.49 0-70.427 19.703-89.243 49.09C275.607 131.383 298.961 163 306.545 200z" ]
                []
            ]
        ]


checkMark : Element.Color -> Element msg
checkMark color =
    toElement color "0, 0, 512, 512" <|
        [ path [ d "M256 48C141.6 48 48 141.6 48 256s93.6 208 208 208 208-93.6 208-208S370.4 48 256 48zm-42.7 318.9L106.7 260.3l29.9-29.9 76.8 76.8 162.1-162.1 29.9 29.9-192.1 191.9z" ]
            []
        ]


ribbon : Element msg
ribbon =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M256 32c-70.7 0-128 57.3-128 128s57.3 128 128 128 128-57.3 128-128S326.7 32 256 32zm0 208c-44.2 0-80-35.8-80-80s35.8-80 80-80 80 35.8 80 80-35.8 80-80 80zM193.7 307.4c-19.1-8.1-36.2-19.6-50.8-34.3-1.4-1.4-2.8-2.8-4.1-4.3L64 400h96l48 80 48-105.8 25.5-56.2c-8.4 1.3-16.9 2-25.5 2-21.6 0-42.5-4.2-62.3-12.6zM373.3 268.9c-1.3 1.4-2.7 2.9-4.1 4.3-14.6 14.6-31.7 26.2-50.7 34.2L294 361.2l-21.9 48.4L304 480l48-80h96l-74.7-131.1z" ]
            []
        ]


person : Element msg
person =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M256 48C141.1 48 48 141.1 48 256s93.1 208 208 208 208-93.1 208-208S370.9 48 256 48zm72 152c13.3 0 24 10.7 24 24s-10.7 24-24 24-24-10.7-24-24 10.7-24 24-24zm-144 0c13.3 0 24 10.7 24 24s-10.7 24-24 24-24-10.7-24-24 10.7-24 24-24zm72 169c-44.7 0-82.3-29.9-94.2-70.7-1.5-5.1 2.3-10.3 7.7-10.3h172.9c5.3 0 9.2 5.1 7.7 10.3-11.8 40.8-49.4 70.7-94.1 70.7z" ]
            []
        ]


wrench : Element msg
wrench =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M441.1 131.1l-44.9 45.1c-.9.9-2.3 1.3-3.5 1.1l-46.4-8.4c-1.6-.3-2.9-1.6-3.2-3.2l-8.3-46.4c-.2-1.3.2-2.6 1.1-3.5l44.8-45c3.5-3.5 3-9.3-1-12.1-10.1-7.2-22.1-10.7-31.8-10.7-.7 0-1.4 0-2 .1-12.5.7-39.3 7.7-60 29.7-20.1 21.2-41.1 60.6-22.5 104.5 2.2 5.3 4.7 12.3-2.7 19.7C253.1 209.4 61 390.3 61 390.3c-18 15.5-16.7 44.2-.1 60.9 8.5 8.4 20 12.8 31.3 12.8 11.1 0 21.9-4.2 29.6-13.1 0 0 179.4-191.1 188.2-199.8 4-3.9 7.7-5.1 11.1-5.1 3.3 0 6.3 1.2 8.6 2.4 9.9 5.1 21 7.4 32.4 7.4 26.8 0 55-12.4 72.2-29.6 24.4-24.4 28.9-48 29.6-60.1.6-9.9-2.2-22.6-10.7-34.2-2.9-3.8-8.6-4.2-12.1-.8zM102.5 429.3c-5.5 5.4-14.4 5.4-19.9 0-5.4-5.5-5.4-14.4 0-19.9 5.5-5.4 14.4-5.4 19.9 0 5.4 5.6 5.4 14.5 0 19.9z" ]
            []
        ]


becker : Element msg
becker =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M437.4 354.4L320.7 159.9c-.4-.6-.6-1.3-.6-2.1V80c0-2.2 1.8-4 4-4 6.6 0 12-5.4 12-12v-4c0-6.6-5.4-12-12-12H187.8c-6.6 0-12 5.4-12 12v4c0 6.6 5.4 12 12 12 2.2 0 4 1.8 4 4v77.9c0 .7-.2 1.4-.6 2L75.7 354.4c-8.4 15.8-12.5 31.4-12.1 45.6 1.1 36.5 28.8 64 65.2 64h256.6c36.4 0 62.3-27.6 63.2-64 .2-14.2-2.7-29.7-11.2-45.6zM161.8 288c-6.2 0-10.1-6.8-6.9-12.1l60.5-101.7c2.9-4.9 4.5-10.6 4.5-16.3V80c0-1.4-.1-2.7-.2-4h72.7c-.2 1.3-.2 2.6-.2 4v77.9c0 5.8 1.6 11.5 4.6 16.4l60.4 101.6c3.2 5.3-.7 12.1-6.9 12.1H161.8z" ]
            []
        ]


key : Element msg
key =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M344.8 218.1c-13 0-25.6 0-37.4 4.1-50.6-43.1-184.3-156.9-194.5-167.5-4.7-4.9-9.9-6.7-15-6.7-8.5 0-16.7 5.2-21.3 9.6-6.9 6.6-33 34.8-28 40 15 15.4 19 18.5 25.2 24.8 9.3 9.5 28.3-1 36 2.3 7.6 3.3 9.2 6.8 10.4 12.5s-2.9 15.8-3 23.7c-.1 8.3 3.4 12.8 9.2 19 4.6 5 8.9 8.6 15.6 8.7 9 .2 20.9-12.8 30.4-3.1s-6.2 23.7-5 34 15.5 22.8 21.6 24.1c6.1 1.3 21.8-11.7 30.7-9.7 3 .7 10 6.8 11 11.4s-6.9 25-5.9 29.6c1.2 5.6 7.1 12.1 10.4 17.4-6.7 15.5-9.4 29.6-9.4 47.7 0 68.5 53.4 124 119.2 124s119-55.5 119-124-53.4-121.9-119.2-121.9zM368 400c-17.7 0-32-14.3-32-32s14.3-32 32-32 32 14.3 32 32-14.3 32-32 32z" ]
            []
        ]


trash : Element msg
trash =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M128 405.429C128 428.846 147.198 448 170.667 448h170.667C364.802 448 384 428.846 384 405.429V160H128v245.429zM416 96h-80l-26.785-32H202.786L176 96H96v32h320V96z" ]
            []
        ]


save : Element msg
save =
    toElement Color.Dracula.white
        "0 0 512 512"
        [ path [ d "M272 64h-16c-4.4 0-8 3.6-8 8v72c0 4.4 7.6 8 12 8h12c4.4 0 8-3.6 8-8V72c0-4.4-3.6-8-8-8z" ]
            []
        , path [ d "M433.9 130.1L382 78.2c-9-9-21.3-14.2-34.1-14.2h-28c-8.8 0-16 7.3-16 16.2v80c0 8.8-7.2 16-16 16H160c-8.8 0-16-7.2-16-16v-80c0-8.8-7.2-16.2-16-16.2H96c-17.6 0-32 14.4-32 32v320c0 17.6 14.4 32 32 32h320c17.6 0 32-14.4 32-32V164c0-12.7-5.1-24.9-14.1-33.9zM322 400.1c0 8.8-8 16-17.8 16H143.8c-9.8 0-17.8-7.2-17.8-16v-96c0-8.8 8-16 17.8-16h160.4c9.8 0 17.8 7.2 17.8 16v96z" ]
            []
        ]
