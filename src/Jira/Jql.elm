module Jira.Jql exposing
    ( Jql, fieldEqualsString, fieldEqualsExpression, literalStringToExpression
    , forProject
    )

{-| JQL (Jira Query Language) expression building helpers


# Generic

@docs Jql, fieldEqualsString, fieldEqualsExpression, literalStringToExpression


# Helpful expressions

@docs forProject

-}

import Jira.Api
import Jira.JqlInternal


{-| JQL query expression
-}
type alias Jql =
    Jira.JqlInternal.Jql


replace : String -> String -> String -> String
replace search replacement source =
    String.split search source
        |> String.join replacement


{-| Turn string to a escaped Jql string expression

    literalStringToExpression "abc" == "\"abc\""

    literalStringToExpression "\"Foo\" Bar" == "\"\\\"Foo\\\" Bar\""

-}
literalStringToExpression : String -> Jql
literalStringToExpression string =
    "\"" ++ replace "\"" "\\\"" string ++ "\""


{-| Build an expression which compares issue field to a string
-}
fieldEqualsString : String -> String -> Jql
fieldEqualsString field string =
    fieldEqualsExpression field (literalStringToExpression string)


{-| Build an expression which compares issue field to an another expression
-}
fieldEqualsExpression : String -> String -> Jql
fieldEqualsExpression field expression =
    field ++ " = " ++ expression


{-| An expression which filters issues from the given Project
-}
forProject : Jira.Api.Project -> Jql
forProject project =
    project
        |> Jira.Api.getProjectData
        |> .id
        |> fieldEqualsExpression "project"
