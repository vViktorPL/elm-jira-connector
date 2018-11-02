module Jira.Api exposing
    ( ApiCallError
    , Cred
    , Project
    , apiErrorToString
    , createAnonymousCred
    , createBasicAuthCred
    , getAllProjects
    , getProjectData
    , getProjects
    )

import Base64
import Http
import Jira.Pagination exposing (Page, PageRequest, pageDecoder, pageRequestToQueryParams)
import Json.Decode as D
import Regex
import Task exposing (Task)
import Url.Builder


type JiraUrl
    = JiraUrl String


type Cred
    = Anonymous JiraUrl
    | BasicAuth JiraUrl ( String, String )


type alias AvatarDimensions =
    String


type alias AvatarUrl =
    String


type alias Avatar =
    ( AvatarDimensions, AvatarUrl )


type Project
    = Project ProjectData


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


type ApiCallError
    = HttpError Http.Error
    | InvalidCreds String


apiErrorToString : ApiCallError -> String
apiErrorToString error =
    case error of
        HttpError (Http.BadPayload errDetails _) ->
            errDetails

        HttpError _ ->
            "Http error"

        InvalidCreds errDetails ->
            errDetails


getProjectData : Project -> ProjectData
getProjectData (Project projectData) =
    projectData


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


createAnonymousCred : String -> Result String Cred
createAnonymousCred jiraUrl =
    createJiraUrlFromString jiraUrl
        |> Result.map Anonymous


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



-- API TASKS


type alias ApiTask response =
    Task ApiCallError response


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


getAllProjects : Cred -> ApiTask (List Project)
getAllProjects cred =
    getAll getProjects cred
