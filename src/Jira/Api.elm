module Jira.Api exposing
    ( Cred, createAnonymousCred, createBasicAuthCred
    , Project, Issue
    , ApiTask, getProjects, getAllProjects, getIssues, getFullIssues
    , allFields, allFieldsExcept
    , getProjectData, getIssueId, getIssueKey, getIssueFields
    , ApiCallError, apiErrorToString
    )

{-| This library helps in communication with Jira REST API by exposing API requests as
straightforward tasks and Jira resources data into some opaque types.


# Credentials

@docs Cred, createAnonymousCred, createBasicAuthCred


# Jira entities

@docs Project, Issue


# API call tasks

@docs ApiTask, getProjects, getAllProjects, getIssues, getFullIssues


# API call helpers

@docs allFields, allFieldsExcept


# Data getters

@docs getProjectData, getIssueId, getIssueKey, getIssueFields


# Errors

@docs ApiCallError, apiErrorToString

-}

import Base64
import Http
import Jira.JqlInternal exposing (Jql)
import Jira.Pagination exposing (Page, PageRequest, pageDecoder, pageRequestToQueryParams)
import Json.Decode as D
import Regex
import Task exposing (Task)
import Url.Builder


type JiraUrl
    = JiraUrl String


{-| Authentication credentials
-}
type Cred
    = Anonymous JiraUrl
    | BasicAuth JiraUrl ( String, String )


type alias AvatarDimensions =
    String


type alias AvatarUrl =
    String


type alias Avatar =
    ( AvatarDimensions, AvatarUrl )


{-| Jira Project
-}
type Project
    = Project ProjectData


{-| Jira Issue
-}
type Issue
    = Issue IssueData


type alias IssueData =
    { id : String
    , key : String
    , fields : D.Value
    }


type alias ProjectData =
    { avatarUrls : List Avatar
    , expand : String
    , id : String
    , name : String
    , key : String
    , projectTypeKey : String
    , self : String
    , simplified : Bool
    }


{-| Error from api call request task
-}
type ApiCallError
    = HttpError Http.Error
    | InvalidCreds String


{-| Convert API call error to a simple string
-}
apiErrorToString : ApiCallError -> String
apiErrorToString error =
    case error of
        HttpError (Http.BadPayload errDetails _) ->
            errDetails

        HttpError _ ->
            "Http error"

        InvalidCreds errDetails ->
            errDetails


{-| Extract raw data from Project
-}
getProjectData : Project -> ProjectData
getProjectData (Project projectData) =
    projectData


{-| Get issue id
-}
getIssueId : Issue -> String
getIssueId (Issue issue) =
    issue.id


{-| Get issue key
-}
getIssueKey : Issue -> String
getIssueKey (Issue issue) =
    issue.key


{-| Get issue fields.
Note: as structure of available fields vary depending on requested fields configuration, you have to decode the fields
that you are interested in.
-}
getIssueFields : Issue -> D.Value
getIssueFields (Issue issue) =
    issue.fields


createJiraUrlFromString : String -> Result String JiraUrl
createJiraUrlFromString url =
    let
        urlPattern =
            Maybe.withDefault Regex.never <|
                Regex.fromStringWith
                    { caseInsensitive = True, multiline = False }
                    "^(?:(https?://)?)([-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*))\\/?$"
    in
    case List.map .match (Regex.find urlPattern url) of
        [ host ] ->
            Ok (JiraUrl ("https://" ++ host ++ "/rest/api/3"))

        _ ->
            Err "Invalid URL"


{-| Create credentials for publicly open Jira (as an anonymous user)
-}
createAnonymousCred : String -> Result String Cred
createAnonymousCred jiraUrl =
    createJiraUrlFromString jiraUrl
        |> Result.map Anonymous


{-| Create credentials using basic auth method
-}
createBasicAuthCred : String -> ( String, String ) -> Result String Cred
createBasicAuthCred urlToJira ( username, password ) =
    createJiraUrlFromString urlToJira
        |> Result.andThen
            (\jiraUrl ->
                case ( String.isEmpty username, String.isEmpty password ) of
                    ( True, True ) ->
                        Err "username and password pair cannot be empty"

                    ( True, False ) ->
                        Err "username cannot be empty"

                    ( False, True ) ->
                        Err "password cannot be empty"

                    ( False, False ) ->
                        Ok (BasicAuth jiraUrl ( username, password ))
            )


apiGet : D.Decoder response -> Cred -> String -> ApiTask response
apiGet decoder cred resource =
    { method = "GET"
    , headers = []
    , url = resource
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }
        |> authorizeApiRequestConfig cred
        |> Http.request
        |> Http.toTask
        |> Task.mapError
            (\err ->
                case err of
                    Http.BadStatus response ->
                        if response.status.code == 401 then
                            InvalidCreds "Invalid user or password"

                        else
                            HttpError (Http.BadStatus response)

                    httpError ->
                        HttpError httpError
            )


type alias RequestConfig a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Float
    , withCredentials : Bool
    }


authorizeApiRequestConfig : Cred -> RequestConfig a -> RequestConfig a
authorizeApiRequestConfig cred request =
    case cred of
        Anonymous (JiraUrl jiraUrl) ->
            { request | url = jiraUrl ++ request.url }

        BasicAuth (JiraUrl jiraUrl) basicAuthCreds ->
            { request
                | url = jiraUrl ++ request.url
                , headers = request.headers ++ [ basicAuthHeader basicAuthCreds ]
            }


basicAuthHeader : ( String, String ) -> Http.Header
basicAuthHeader ( username, password ) =
    Http.header "Authorization" <| "Basic " ++ Base64.encode (username ++ ":" ++ password)



-- DECODERS


projectDecoder : D.Decoder Project
projectDecoder =
    D.map Project <|
        D.map8 ProjectData
            (D.field "avatarUrls" (D.keyValuePairs D.string))
            (D.field "expand" D.string)
            (D.field "id" D.string)
            (D.field "name" D.string)
            (D.field "key" D.string)
            (D.field "projectTypeKey" D.string)
            (D.field "self" D.string)
            (D.field "simplified" D.bool)


issueDecoder : D.Decoder Issue
issueDecoder =
    D.map Issue <|
        D.map3 IssueData
            (D.field "id" D.string)
            (D.field "key" D.string)
            (D.field "fields" D.value)



-- API TASKS


{-| Jira API call task
-}
type alias ApiTask response =
    Task ApiCallError response


{-| Get page of projects
-}
getProjects : Cred -> PageRequest -> ApiTask (Page Project)
getProjects cred pagination =
    let
        decoder =
            pageDecoder projectDecoder

        queryParams =
            pageRequestToQueryParams pagination

        resource =
            Url.Builder.absolute [ "project", "search" ] queryParams
    in
    apiGet decoder cred resource


{-| Search for issues using Jql with and scoping available fields per issue
-}
getIssues : Cred -> PageRequest -> Jql -> List String -> ApiTask (Page Issue)
getIssues cred pagination jql fields =
    let
        decoder =
            pageDecoder issueDecoder

        queryParams =
            Url.Builder.string "jql" jql
                :: Url.Builder.string "fields" (String.join "," fields)
                :: Url.Builder.string "properties" "id,key,summary"
                :: pageRequestToQueryParams pagination

        resource =
            Url.Builder.absolute [ "search" ] queryParams
    in
    apiGet decoder cred resource


{-| Search for issues using Jql with all fields per issue
-}
getFullIssues : Cred -> PageRequest -> Jql -> ApiTask (Page Issue)
getFullIssues cred pagination jql =
    getIssues cred pagination jql allFields


{-| Fields scope where all fields are requested
-}
allFields : List String
allFields =
    [ "*all" ]


{-| Fields scope where all fields except provided ones are requested
-}
allFieldsExcept : List String -> List String
allFieldsExcept exceptions =
    allFields ++ List.map ((++) "-") exceptions


requestPageForFetchAllTask =
    Jira.Pagination.pageRequest (Jira.Pagination.paginationConfig 500) 1


getAll : (Cred -> PageRequest -> ApiTask (Page a)) -> Cred -> ApiTask (List a)
getAll pageFetchTask cred =
    pageFetchTask cred requestPageForFetchAllTask
        |> Task.andThen
            (\firstPageResult ->
                let
                    firstPageItems =
                        Jira.Pagination.getItems firstPageResult

                    pagesCount =
                        Jira.Pagination.totalPages firstPageResult

                    paginationConfig =
                        Jira.Pagination.paginationConfig (List.length firstPageItems)
                in
                Task.sequence
                    (Task.succeed firstPageItems
                        :: (List.range 2 pagesCount
                                |> List.map (Jira.Pagination.pageRequest paginationConfig)
                                |> List.map (\pageRequest -> pageFetchTask cred pageRequest)
                                |> List.map (Task.map Jira.Pagination.getItems)
                           )
                    )
                    |> Task.map List.concat
            )


{-| Get all projects
-}
getAllProjects : Cred -> ApiTask (List Project)
getAllProjects cred =
    getAll getProjects cred
