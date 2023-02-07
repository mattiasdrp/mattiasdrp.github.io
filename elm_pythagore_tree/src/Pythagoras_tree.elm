module Pythagoras_tree exposing (..)

import Color
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


next_2_squares : Model -> Coords -> Coords -> ( Square, Square )
next_2_squares model c1 c2 =
    let
        alpha =
            degrees (SingleSlider.fetchValue model.alpha)

        beta =
            arg c1 c2

        dc1c2 =
            distance c1 c2

        -- Distance between h and c1
        hc1 =
            dc1c2 * cos alpha ^ 2

        -- Distance between a and c1
        ac1 =
            hc1 / cos alpha

        xa =
            c1.x + ac1 * cos (alpha + beta)

        ya =
            c1.y - ac1 * sin (alpha + beta)

        dx1 =
            xa - c1.x

        dy1 =
            c1.y - ya

        -- Since svg goes up to bottom, ya < c1.y
        xb =
            c1.x - dy1

        yb =
            c1.y - dx1

        xc =
            xa - dy1

        yc =
            ya - dx1

        dx2 =
            c2.x - xa

        dy2 =
            ya - c2.y

        xd =
            xa - dy2

        yd =
            ya - dx2

        xe =
            c2.x - dy2

        ye =
            c2.y - dx2

        square1 =
            { c1 = { x = xb, y = yb }, c2 = { x = xc, y = yc }, c3 = { x = xa, y = ya }, c4 = c1 }

        square2 =
            { c1 = { x = xd, y = yd }, c2 = { x = xe, y = ye }, c3 = c2, c4 = { x = xa, y = ya } }
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


aux_generate_squares : Model -> Float -> List Square -> List Square -> List Square
aux_generate_squares model depth squares acc =
    if depth == 1 then
        acc

    else
        let
            ( tacc, nacc ) =
                List.foldl
                    (\square ( ttacc, nnacc ) ->
                        let
                            ( s1, s2 ) =
                                next_2_squares model square.c1 square.c2
                        in
                        ( s1 :: s2 :: ttacc
                        , s1 :: s2 :: nnacc
                        )
                    )
                    ( [], acc )
                    squares
        in
        aux_generate_squares model (depth - 1) tacc nacc


generate_squares : Float -> List Square -> List Square
generate_squares model squares =
    aux_generate_squares model model.depth squares squares


view_pythagoras_tree : Float -> Html Msg
view_pythagoras_tree val =
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
            generate_squares val [ square ]
    in
    svg
        [ width (px box_size), height (px box_size) ]
        (List.map square_to_svg squares)
