module Jira.Api exposing
    ( Cred, createAnonymousCred, createBasicAuthCred
    , Project, Issue, Worklog, WorklogRequest, WorklogData, Content
    , ApiTask, getProjects, getAllProjects, getIssues, getFullIssues, addWorklog, addWorklogToIssueByKeyOrId
    , allFields, allFieldsExcept
    , getProjectData, getIssueId, getIssueKey, getIssueFields, getWorklogData
    , contentFromString
    , ApiCallError, apiErrorToString
    )

{-| This library helps in communication with Jira REST API by exposing API requests as
straightforward tasks and Jira resources data into some opaque types.


# Credentials

@docs Cred, createAnonymousCred, createBasicAuthCred


# Jira entities

@docs Project, Issue, Worklog, WorklogRequest, WorklogData, Content


# API call tasks

@docs ApiTask, getProjects, getAllProjects, getIssues, getFullIssues, addWorklog, addWorklogToIssueByKeyOrId


# API call helpers

@docs allFields, allFieldsExcept


# Data getters

@docs getProjectData, getIssueId, getIssueKey, getIssueFields, getWorklogData


# Data creators

@docs contentFromString


# Errors

@docs ApiCallError, apiErrorToString

-}

import Base64
import Http
import Iso8601
import Jira.JqlInternal exposing (Jql)
import Jira.Pagination exposing (Page, PageRequest, pageDecoder, pageRequestToQueryParams)
import Json.Decode as D
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Regex
import Task exposing (Task)
import Time exposing (Posix)
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


{-| Structure used to add new worklog to an issue
-}
type alias WorklogRequest =
    { started : Posix
    , timeSpentSeconds : Int
    , comment : Maybe Content
    }


{-| Issue worklog entry
-}
type Worklog
    = Worklog WorklogData


{-| Persisted worklog details
-}
type alias WorklogData =
    { id : String
    , issueId : String
    , created : Posix
    , started : Posix
    , updated : Posix
    , timeSpent : String
    , timeSpentSeconds : Int
    , author : D.Value
    , updateAuthor : D.Value
    }


{-| Content which is used across comments, descriptions etc
-}
type Content
    = SimpleText String


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
    Ok <|
        JiraUrl
            (if String.endsWith "/" url then
                url ++ "rest/api/3"

             else
                url ++ "/rest/api/3"
            )


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


{-| Create text content
-}
contentFromString : String -> Content
contentFromString string =
    SimpleText string


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


apiPost : D.Decoder response -> Cred -> String -> E.Value -> ApiTask response
apiPost decoder cred resource bodyValue =
    { method = "POST"
    , headers = []
    , url = resource
    , body = Http.jsonBody bodyValue
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


worklogDecoder : D.Decoder Worklog
worklogDecoder =
    D.map Worklog
        (D.succeed WorklogData
            |> required "id" D.string
            |> required "issueId" D.string
            |> required "created" posixDecoder
            |> required "started" posixDecoder
            |> required "updated" posixDecoder
            |> required "timeSpent" D.string
            |> required "timeSpentSeconds" D.int
            |> required "author" D.value
            |> required "updateAuthor" D.value
        )


posixDecoder : D.Decoder Posix
posixDecoder =
    D.string
        |> D.map
            (\string ->
                if String.right 1 string == "Z" then
                    string

                else
                    -- fix for timezones i.e. "+0100" -> "+01:00"
                    String.dropRight 2 string ++ ":" ++ String.right 2 string
            )
        |> D.map Iso8601.toTime
        |> D.andThen
            (\parsingResult ->
                case parsingResult of
                    Ok posix ->
                        D.succeed posix

                    Err _ ->
                        D.fail "Invalid date string format"
            )



-- ENCODING


encodeContent : Content -> E.Value
encodeContent content =
    case content of
        SimpleText text ->
            E.object
                [ ( "type", E.string "doc" )
                , ( "version", E.int 1 )
                , ( "content"
                  , E.list identity
                        [ E.object
                            [ ( "type", E.string "paragraph" )
                            , ( "content"
                              , E.list identity
                                    [ E.object
                                        [ ( "type", E.string "text" )
                                        , ( "text", E.string text )
                                        ]
                                    ]
                              )
                            ]
                        ]
                  )
                ]


encodeWorklogRequest : WorklogRequest -> E.Value
encodeWorklogRequest worklogRequest =
    E.object
        ([ ( "started", encodePosix worklogRequest.started )
         , ( "timeSpentSeconds", E.int worklogRequest.timeSpentSeconds )
         ]
            ++ (case worklogRequest.comment of
                    Just comment ->
                        [ ( "comment", encodeContent comment ) ]

                    Nothing ->
                        []
               )
        )


encodePosix : Posix -> E.Value
encodePosix posix =
    posix
        |> Iso8601.fromTime
        |> String.replace "Z" "+0000"
        -- for some reason JIRA REST API does not accept "Z" timezone
        |> E.string



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


{-| Add worklog to an issue

    addWorklog cred
        issue
        { started = Posix.millisToPosix 1543173302785
        , timeSpentSeconds = 3600
        , comment = Just (contentFromString "Some comment")
        }

-}
addWorklog : Cred -> Issue -> WorklogRequest -> ApiTask Worklog
addWorklog cred issue worklogRequest =
    addWorklogToIssueByKeyOrId cred (getIssueKey issue) worklogRequest


{-| Add worklog to an issue by it's key or id:

    addWorklog cred
        "EXA-1"
        { started = Time.millisToPosix 1543173302785
        , timeSpentSeconds = 3600
        , comment = Just (contentFromString "Some comment")
        }

-}
addWorklogToIssueByKeyOrId : Cred -> String -> WorklogRequest -> ApiTask Worklog
addWorklogToIssueByKeyOrId cred issueKeyOrId worklogRequest =
    apiPost
        worklogDecoder
        cred
        ("/issue/" ++ issueKeyOrId ++ "/worklog")
        (encodeWorklogRequest worklogRequest)


{-| Get data from worklog
-}
getWorklogData : Worklog -> WorklogData
getWorklogData (Worklog worklogData) =
    worklogData
