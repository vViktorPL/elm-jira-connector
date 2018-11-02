module Jira.Api exposing (Cred, ApiCallError, Project, createAnonymousCred, createBasicAuthCred, getProjectData, getProjects, apiErrorToString)

import Base64
import Http
import Jira.Pagination exposing (Page, PageRequest, pageDecoder, pageRequestToQueryParams)
import Json.Decode as D
import Regex
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


apiGet : ApiMsg msg response -> D.Decoder response -> Cred -> String -> Cmd msg
apiGet msg decoder cred resource =
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
        |> Http.send (\result ->
            case result of
                Ok response -> msg (Ok response)
                Err (Http.BadStatus response) ->
                    if response.status.code == 401 then
                        msg (Err (InvalidCreds "Invalid user or password"))
                    else
                        msg (Err (HttpError (Http.BadStatus response)))
                Err httpError -> msg (Err (HttpError httpError))
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

-- API COMMANDS

type alias ApiMsg msg response =
    Result ApiCallError response -> msg

getProjects : ApiMsg msg (Page Project) -> Cred -> PageRequest -> Cmd msg
getProjects msg cred pagination =
    let
        decoder =
            pageDecoder projectDecoder

        queryParams =
            pageRequestToQueryParams pagination

        resource =
            Url.Builder.absolute [ "project", "search" ] queryParams
    in
    apiGet msg decoder cred resource

