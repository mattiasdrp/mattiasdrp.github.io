module Pythagoras_tree exposing (view_pythagoras_tree)

import Color
import Html exposing (..)
import Svg exposing (Svg)
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Types exposing (..)


type alias Coords =
    { x : Float, y : Float }


pair_coord : Coords -> ( Float, Float )
pair_coord { x, y } =
    ( x, y )


type alias Square =
    { c1 : Coords
    , c2 : Coords
    , c3 : Coords
    , c4 : Coords
    }


square_to_svg : Square -> Svg msg
square_to_svg { c1, c2, c3, c4 } =
    polygon [ points [ pair_coord c1, pair_coord c2, pair_coord c3, pair_coord c4 ], fill PaintNone, stroke (Paint Color.black) ] []


distance : Coords -> Coords -> Float
distance c1 c2 =
    sqrt (((c1.y - c2.y) ^ 2) + ((c1.x - c2.x) ^ 2))


arg : Coords -> Coords -> Float
arg c1 c2 =
    if c2.x == c1.x && c2.y == c1.y then
        0

    else
        atan2
            (c1.y - c2.y)
            (c2.x - c1.x)


next_2_squares : Float -> Float -> Coords -> Coords -> ( Square, Square )
next_2_squares angle1 angle2 a b =
    let
        -- Angle BAC
        alpha =
            degrees angle1

        -- Angle ACB
        delta =
            degrees (180 - angle1 - angle2)

        beta =
            arg a b

        ab =
            distance a b

        -- Distance between a and c1
        ac =
            ab * sin delta / sin (alpha + delta)

        dx1 =
            ac * cos (alpha + beta)

        dy1 =
            ac * sin (alpha + beta)

        xc =
            a.x + dx1

        yc =
            a.y - dy1

        -- Since svg goes up to bottom, ya < c1.y
        xd =
            a.x - dy1

        yd =
            a.y - dx1

        xe =
            xc - dy1

        ye =
            yc - dx1

        dx2 =
            b.x - xc

        dy2 =
            b.y - yc

        xf =
            xc + dy2

        yf =
            yc - dx2

        xg =
            b.x + dy2

        yg =
            b.y - dx2

        square1 =
            { c1 = { x = xd, y = yd }, c2 = { x = xe, y = ye }, c3 = { x = xc, y = yc }, c4 = a }

        square2 =
            { c1 = { x = xf, y = yf }, c2 = { x = xg, y = yg }, c3 = b, c4 = { x = xc, y = yc } }
    in
    ( square1, square2 )



-- CONSTANTS


box_size : number
box_size =
    1000


edge : number
edge =
    100



-- VIEW


aux_generate_squares : Float -> Float -> Float -> List Square -> List Square -> List Square
aux_generate_squares angle1 angle2 depth squares acc =
    if depth == 1 then
        acc

    else
        let
            ( tacc, nacc ) =
                List.foldl
                    (\square ( ttacc, nnacc ) ->
                        let
                            ( s1, s2 ) =
                                next_2_squares angle1 angle2 square.c1 square.c2
                        in
                        ( s1 :: s2 :: ttacc
                        , s1 :: s2 :: nnacc
                        )
                    )
                    ( [], acc )
                    squares
        in
        aux_generate_squares angle1 angle2 (depth - 1) tacc nacc


generate_squares : Float -> Float -> Float -> List Square -> List Square
generate_squares angle1 angle2 depth squares =
    aux_generate_squares angle1 angle2 depth squares squares


view_pythagoras_tree : Float -> Float -> Float -> Html msg
view_pythagoras_tree angle1 angle2 depth =
    let
        square =
            { c1 =
                { x = box_size / 2 - edge / 2, y = box_size - 3 * edge }
            , c2 =
                { x = box_size / 2 + edge / 2, y = box_size - 3 * edge }
            , c3 =
                { x = box_size / 2 + edge / 2, y = box_size - 2 * edge }
            , c4 =
                { x = box_size / 2 - edge / 2, y = box_size - 2 * edge }
            }

        squares =
            generate_squares angle1 angle2 depth [ square ]
    in
    svg
        [ width (px box_size), height (px box_size) ]
        (List.map square_to_svg squares)
