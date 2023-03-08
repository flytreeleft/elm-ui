module Element.Keyed exposing (el, column, row, wrappedRow)

{-| Notes from the `Html.Keyed` on how keyed works:

---

A keyed node helps optimize cases where children are getting added, moved, removed, etc. Common examples include:

  - The user can delete items from a list.
  - The user can create new items in a list.
  - You can sort a list based on name or date or whatever.

When you use a keyed node, every child is paired with a string identifier. This makes it possible for the underlying diffing algorithm to reuse nodes more efficiently.

This means if a key is changed between renders, then the diffing step will be skipped and the node will be forced to rerender.

---

@docs el, column, row, wrappedRow

-}

import Element exposing (Attribute, Element, fill, height, shrink, width)
import Html.Attributes
import Internal.Flag as Flag
import Internal.Model as Internal
import Internal.Style exposing (classes)


{-| -}
el : List (Attribute msg) -> ( String, Element msg ) -> Element msg
el attrs child =
    Internal.element
        Internal.asEl
        Internal.div
        (width shrink
            :: height shrink
            :: attrs
        )
        (Internal.Keyed [ child ])


{-| -}
row : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
row attrs children =
    Internal.element
        Internal.asRow
        Internal.div
        (Internal.htmlClass (classes.contentLeft ++ " " ++ classes.contentCenterY)
            :: width shrink
            :: height shrink
            :: attrs
        )
        (Internal.Keyed children)


{-| -}
column : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
column attrs children =
    Internal.element
        Internal.asColumn
        Internal.div
        (Internal.htmlClass
            (classes.contentTop
                ++ " "
                ++ classes.contentLeft
            )
            :: height shrink
            :: width shrink
            :: attrs
        )
        (Internal.Keyed children)


{-| -}
wrappedRow : List (Attribute msg) -> List ( String, Element msg ) -> Element msg
wrappedRow attrs children =
    let
        ( padded, spaced ) =
            Internal.extractSpacingAndPadding attrs
    in
    case spaced of
        Nothing ->
            Internal.element
                Internal.asRow
                Internal.div
                (Internal.htmlClass
                    (classes.contentLeft
                        ++ " "
                        ++ classes.contentCenterY
                        ++ " "
                        ++ classes.wrapped
                    )
                    :: width shrink
                    :: height shrink
                    :: attrs
                )
                (Internal.Keyed children)

        Just (Internal.Spaced spaceName x y) ->
            let
                newPadding =
                    case padded of
                        Just (Internal.Padding name t r b l) ->
                            if r >= (toFloat x / 2) && b >= (toFloat y / 2) then
                                let
                                    newTop =
                                        t - (toFloat y / 2)

                                    newRight =
                                        r - (toFloat x / 2)

                                    newBottom =
                                        b - (toFloat y / 2)

                                    newLeft =
                                        l - (toFloat x / 2)
                                in
                                Just <|
                                    Internal.StyleClass Flag.padding
                                        (Internal.PaddingStyle
                                            (Internal.paddingNameFloat
                                                newTop
                                                newRight
                                                newBottom
                                                newLeft
                                            )
                                            newTop
                                            newRight
                                            newBottom
                                            newLeft
                                        )

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            case newPadding of
                Just pad ->
                    Internal.element
                        Internal.asRow
                        Internal.div
                        (Internal.htmlClass
                            (classes.contentLeft
                                ++ " "
                                ++ classes.contentCenterY
                                ++ " "
                                ++ classes.wrapped
                            )
                            :: width shrink
                            :: height shrink
                            :: attrs
                            ++ [ pad ]
                        )
                        (Internal.Keyed children)

                Nothing ->
                    -- Not enough space in padding to compensate for spacing
                    let
                        halfX =
                            negate (toFloat x / 2)

                        halfY =
                            negate (toFloat y / 2)
                    in
                    Internal.element
                        Internal.asEl
                        Internal.div
                        attrs
                        (Internal.Unkeyed
                            [ Internal.element
                                Internal.asRow
                                Internal.div
                                (Internal.htmlClass
                                    (classes.contentLeft
                                        ++ " "
                                        ++ classes.contentCenterY
                                        ++ " "
                                        ++ classes.wrapped
                                    )
                                    :: Internal.Attr
                                        (Html.Attributes.style "margin"
                                            (String.fromFloat halfY
                                                ++ "px"
                                                ++ " "
                                                ++ String.fromFloat halfX
                                                ++ "px"
                                            )
                                        )
                                    :: Internal.Attr
                                        (Html.Attributes.style "width"
                                            ("calc(100% + "
                                                ++ String.fromInt x
                                                ++ "px)"
                                            )
                                        )
                                    :: Internal.Attr
                                        (Html.Attributes.style "height"
                                            ("calc(100% + "
                                                ++ String.fromInt y
                                                ++ "px)"
                                            )
                                        )
                                    :: Internal.StyleClass Flag.spacing (Internal.SpacingStyle spaceName x y)
                                    :: []
                                )
                                (Internal.Keyed children)
                            ]
                        )
