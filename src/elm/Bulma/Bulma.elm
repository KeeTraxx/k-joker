module Bulma.Bulma exposing (..)

import Bulma.HtmlExtra exposing (ariaExpanded, ariaHidden, ariaLabel, role)
import Form exposing (FieldState, Form, getFieldAsString)
import Form.Error exposing (ErrorValue(..))
import Form.Input exposing (textArea, textInput)
import Form.Validate exposing (field)
import Html exposing (Attribute, Html, a, button, div, footer, header, input, label, nav, node, p, section, span, table, tbody, text, tfoot, thead)
import Html.Attributes exposing (attribute, checked, class, classList, href, id, target, type_)
import Html.Events exposing (onClick)
import String exposing (fromFloat, fromInt)


bulmaNav : Html msg -> msg -> msg -> Bool -> List (Attribute msg) -> List (Html msg) -> List (Html msg) -> Html msg
bulmaNav brand openMsg closeMsg isOpen attributes startItems endItems =
    nav (List.concat [ [ class "navbar", role "navigation", ariaLabel "main navigation" ], attributes ])
        [ div
            [ class "navbar-brand" ]
            [ brand
            , a
                [ role "button"
                , target "_self"
                , class "navbar-burger"
                , classList [ ( "is-active", isOpen ) ]
                , ariaLabel "menu"
                , ariaExpanded isOpen
                , attribute "data-target" "mainNavBar"
                , onClick
                    (if isOpen then
                        closeMsg

                     else
                        openMsg
                    )
                ]
                [ span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                , span [ ariaHidden True ] []
                ]
            ]
        , div [ id "mainNavBar", class "navbar-menu", classList [ ( "is-active", isOpen ) ] ]
            [ div [ class "navbar-start" ] startItems
            , div [ class "navbar-end" ] endItems
            ]
        ]


bulmaTable : Bool -> Bool -> Bool -> Bool -> List (Html msg) -> List (Html msg) -> List (Html msg) -> Html msg
bulmaTable fullWidth hoverable striped narrow header body footer =
    table
        [ class "table"
        , classList
            [ ( "is-fullwidth", fullWidth )
            , ( "is-hoverable", hoverable )
            , ( "is-striped", striped )
            , ( "is-narrow", narrow )
            ]
        ]
        [ thead [] header
        , tbody [] body
        , tfoot [] footer
        ]


faIcon : String -> Html msg
faIcon iconName =
    node "ion-icon" [ attribute "name" iconName ] []


bulmaIconButton : String -> Maybe String -> msg -> Html msg
bulmaIconButton iconName maybeLabel onClickMsg =
    button [ class "button is-primary", onClick onClickMsg ]
        [ span [ class "icon", class "is-small" ]
            [ faIcon iconName
            ]
        , case maybeLabel of
            Nothing ->
                text ""

            Just label ->
                span [] [ text label ]
        ]


bulmaLinkButton : String -> String -> Html msg
bulmaLinkButton iconName link =
    a [ href link, target "_blank" ]
        [ button [ class "button is-primary" ]
            [ span [ class "icon", class "is-small" ]
                [ faIcon iconName
                ]
            ]
        ]


bulmaModal : String -> List (Html msg) -> List (Html msg) -> msg -> Html msg
bulmaModal title content footerContents closeMsg =
    div [ class "modal is-active" ]
        [ div [ class "modal-background", onClick closeMsg ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ] [ p [ class "modal-card-title" ] [ text title ] ]
            , section [ class "modal-card-body" ] content
            , footer [ class "modal-card-foot" ] footerContents
            ]
        ]


bulmaCheckbox : Bool -> String -> msg -> Html msg
bulmaCheckbox value title msg =
    div [ class "field" ]
        [ div [ class "control" ]
            [ label [ class "checkbox" ]
                [ input [ onClick msg, type_ "checkbox", checked value ] []
                , text title
                ]
            ]
        ]


bulmaFormCheckbox : String -> FieldState e Bool -> Html Form.Msg
bulmaFormCheckbox labelText field =
    div [ class "field" ]
        [ div [ class "control" ]
            [ label [ class "checkbox" ]
                [ Form.Input.checkboxInput field []
                , text labelText
                ]
            ]
        ]


bulmaFormRadio : String -> List ( String, String ) -> FieldState e String -> Html Form.Msg
bulmaFormRadio labelText values field =
    let
        itemHtml ( v, l ) =
            label [ class "radio" ]
                [ Form.Input.radioInput v field [], text l ]
    in
    div [ class "field" ]
        [ div [ class "control" ]
            (List.map itemHtml values)
        ]


bulmaInputText : String -> Form customError output -> Html Form.Msg
bulmaInputText fieldName form =
    div [ class "field" ]
        [ label [ class "label is-small" ] [ text fieldName ]
        , div [ class "control" ]
            [ textInput (Form.getFieldAsString fieldName form) [ class "input is-small", classList [ ( "is-danger", hasError (Form.getFieldAsString fieldName form) ) ] ]
            ]
        , errorFor (getFieldAsString fieldName form)
        ]


bulmaTextArea : String -> Form customError output -> Html Form.Msg
bulmaTextArea fieldName form =
    div [ class "field" ]
        [ label [ class "label is-small" ] [ text fieldName ]
        , div [ class "control" ]
            [ textArea (Form.getFieldAsString fieldName form) [ class "textarea is-small", classList [ ( "is-danger", hasError (Form.getFieldAsString fieldName form) ) ] ]
            ]
        , errorFor (getFieldAsString fieldName form)
        ]


bulmaError : String -> Html Form.Msg
bulmaError errorMessage =
    p [ class "help is-danger" ]
        [ text errorMessage
        ]


hasError : Form.FieldState customError String -> Bool
hasError field =
    case field.error of
        Just _ ->
            True

        Nothing ->
            False


errorFor : Form.FieldState customError String -> Html Form.Msg
errorFor field =
    if field.isChanged then
        case field.error of
            Just error ->
                case error of
                    InvalidFloat ->
                        bulmaError "Please enter a decimal number"

                    Form.Error.Empty ->
                        bulmaError "Field is required"

                    InvalidString ->
                        bulmaError "Field must be a string"

                    InvalidEmail ->
                        bulmaError "Please enter a valid email address"

                    InvalidFormat ->
                        bulmaError "Invalid format"

                    InvalidInt ->
                        bulmaError "Please enter an integer"

                    InvalidBool ->
                        bulmaError "Please enter a boolean"

                    SmallerIntThan int ->
                        bulmaError <| "Must be smaller than " ++ fromInt int

                    GreaterIntThan int ->
                        bulmaError <| "Must be greater than " ++ fromInt int

                    SmallerFloatThan f ->
                        bulmaError <| "Must be smaller than " ++ fromFloat f

                    GreaterFloatThan f ->
                        bulmaError <| "Must be greater than " ++ fromFloat f

                    ShorterStringThan l ->
                        bulmaError <| "Length shorter than " ++ fromInt l

                    LongerStringThan l ->
                        bulmaError <| "Length longer than " ++ fromInt l

                    NotIncludedIn ->
                        bulmaError "Not included in"

                    CustomError _ ->
                        bulmaError <| "Custom error"

            Nothing ->
                text ""

    else
        text ""


bulmaIconWithText : String -> String -> Html msg
bulmaIconWithText iconName label =
    span [ class "icon-text" ]
        [ span [ class "icon" ] [ faIcon iconName ]
        , span [] [ text label ]
        ]
