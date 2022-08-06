module Bulma.HtmlExtra exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


role : String -> Attribute msg
role input =
    attribute "role" input


ariaLabel : String -> Attribute msg
ariaLabel input =
    attribute "aria-label" input


ariaExpanded : Bool -> Html.Attribute msg
ariaExpanded b =
    attribute "aria-expanded" (toString b)


toString : Bool -> String
toString b =
    if b then
        "true"

    else
        "false"


ariaHidden : Bool -> Html.Attribute msg
ariaHidden b =
    attribute "aria-hidden" (toString b)
