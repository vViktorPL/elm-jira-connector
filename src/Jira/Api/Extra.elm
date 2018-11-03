module Jira.Api.Extra exposing (getIssueNames, decodeIssueSummary)


{-| This module provides some extra helpers for Jira Api calls

# Api calls

@docs getIssueNames

# Data decoding

Some data cannot be extracted directly as it may be missing depending on fields that have been listed in the request.
For such case, you have to use Issue decoding functions.

@docs decodeIssueSummary

-}

import Task
import Json.Decode as D
import Jira.Api exposing (Cred, ApiTask, Issue, getIssues, getIssueKey, getIssueFields, getIssueId)
import Jira.Pagination exposing (PageRequest, Page)
import Jira.JqlInternal exposing (Jql)

{-| Search for issues using Jql but fetch human readable names together with their ids like:

    ("[EXA-1] Some issue", "10000")
    ("[EXA-2] Another one", "10001")

It may be useful in case you want represent a list of issues as a drop down list.
-}
getIssueNames : Cred -> PageRequest -> Jql -> ApiTask (Page (String, String))
getIssueNames cred pagination jql =
    getIssues cred pagination jql ["summary"]
        |> Task.map
            ( Jira.Pagination.map
                (\issue ->
                    let
                        prefix = "[" ++ (getIssueKey issue) ++ "]"
                    in
                    case decodeIssueSummary issue of
                        Ok summary -> ( prefix ++ " " ++ summary, getIssueId issue )
                        Err _ -> ( prefix, getIssueId issue )

                )
            )


{-| Decode issue summary
-}
decodeIssueSummary : Issue -> Result D.Error String
decodeIssueSummary issue = (getIssueFields issue)
    |> D.decodeValue (D.field "summary" D.string)