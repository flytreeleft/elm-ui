module Element.Transition exposing
    ( with, property
    , delay, duration
    , linear, ease, easeIn, easeOut, easeInOut, cubic
    )

{-|

    Element.Transition.with
        [ Element.Transition.property "box-shadow"
            [ Element.Transition.duration 0.28
            , Element.Transition.delay 0
            , Element.Transition.cubic 0.4 0 0.2 1
            ]
        ]

@docs with

## Transition property

@docs property

## Transition Delay and Duration

@docs delay, duration

## Transition Easing Function

@docs linear, ease, easeIn, easeOut, easeInOut, cubic

-}

import Element exposing (Attribute)
import Internal.Flag as Flag
import Internal.Model as Internal
import Hex
import Murmur3


type Option =
    Delay Float
    | Duration Float
    | Property Property
    | EasingFunction EasingFunction


type Property =
    Prop String (List Option)


{-| Easing Function

    https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function

-}
type EasingFunction =
    Linear
    | Ease
    | EaseIn
    | EaseOut
    | EaseInOut
    | Cubic Float Float Float Float


{-| -}
with : List Property -> Attribute msg
with properties =
    let
        transition_ =
            properties
                |> List.map
                    (\(Prop name options) ->
                        name ++ " " ++ (joinOpitons options)
                    )
                |> List.sort
                |> String.join ", "

        className =
            transition_
                |> Murmur3.hashString 2003012614
                |> Hex.toString
                |> (++) "trans-"
    in
    Internal.StyleClass (Flag.flag 124) <|
        Internal.Single
            className
            "transition"
            transition_


{-| -}
property : String -> List Option -> Property
property =
    Prop


{-| -}
delay : Float -> Option
delay seconds =
    Delay seconds


{-| -}
duration : Float -> Option
duration seconds =
    Duration seconds


{-| -}
linear : Option
linear =
    -- = cubic-bezier(0.0, 0.0, 1.0, 1.0)
    EasingFunction Linear


{-| -}
ease : Option
ease =
    -- = cubic-bezier(0.25, 0.1, 0.25, 1.0)
    EasingFunction Ease


{-| -}
easeIn : Option
easeIn =
    -- = cubic-bezier(0.42, 0, 1.0, 1.0)
    EasingFunction EaseIn


{-| -}
easeOut : Option
easeOut =
    -- = cubic-bezier(0, 0, 0.58, 1.0)
    EasingFunction EaseOut


{-| -}
easeInOut : Option
easeInOut =
    -- = cubic-bezier(0.42, 0, 0.58, 1.0)
    EasingFunction EaseInOut


{-| -}
cubic : Float -> Float -> Float -> Float -> Option
cubic a b c d =
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function#cubic_b%C3%A9zier_easing_function
    EasingFunction (Cubic a b c d)


-- Local


joinOpitons : List Option -> String
joinOpitons options =
    -- duration | easing function | delay
    options
        |> List.foldl
            (\opt result ->
                case opt of
                    Duration sec ->
                        ( String.fromFloat sec ++ "s" )
                            :: result

                    Delay sec ->
                        result ++ [ String.fromFloat sec ++ "s" ]

                    EasingFunction fn ->
                        let
                            val =
                                case fn of
                                    Cubic a b c d ->
                                        "cubic-bezier("
                                            ++ ( [ a, b, c, d ]
                                                    |> List.map String.fromFloat
                                                    |> String.join ","
                                                )
                                            ++ ")"

                                    Linear ->
                                        "linear"

                                    Ease ->
                                        "ease"

                                    EaseIn ->
                                        "ease-in"

                                    EaseOut ->
                                        "ease-out"

                                    EaseInOut ->
                                        "ease-in-out"
                        in
                        case result of
                            first :: last ->
                                first :: val :: last
                            _ ->
                                result ++ [ val ]

                    _ ->
                        result

            )
            []
        |> String.join " "
