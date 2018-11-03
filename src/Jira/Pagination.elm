module Jira.Pagination exposing
    ( PaginationConfig, PageRequest, paginationConfig, pageRequest, pageRequestToQueryParams
    , Page, getItems, isLast, nextPage, pageNumber, totalPages, pageDecoder, map
    )

{-| This module wraps paginated responses from Jira API


# Page requests

@docs PaginationConfig, PageRequest, paginationConfig, pageRequest, pageRequestToQueryParams


# Pages

@docs Page, getItems, isLast, nextPage, pageNumber, totalPages, pageDecoder, map

-}

import Json.Decode as D exposing (Decoder)
import Url.Builder exposing (QueryParameter)


{-| Represents data that is required to build a request for a specific page
-}
type PageRequest
    = PageRequest Int Int


{-| Pagination configuration which is just the limit of items per page.
-}
type PaginationConfig
    = PaginationConfig Int


type alias PageInternals item =
    { maxResults : Int
    , startAt : Int
    , total : Int
    , isLast : Bool
    , values : List item
    }


{-| Wraps up data from a decoded response of specific page request
-}
type Page item
    = Page (PageInternals item)


{-| Is the page the last one?
-}
isLast : Page a -> Bool
isLast (Page page) =
    page.isLast


{-| Extract total number of pages
-}
totalPages : Page a -> Int
totalPages (Page page) =
    ceiling (toFloat page.total / toFloat page.maxResults)


{-| What is the limit of the results per page?
-}
pageSize : Page a -> Int
pageSize (Page page) =
    page.maxResults


{-| Get PageRequest for the page which is next to the provided one.
There could be no next page hence Maybe return type.
-}
nextPage : Page a -> Maybe PageRequest
nextPage page =
    case isLast page of
        True ->
            Nothing

        False ->
            let
                config =
                    paginationConfig (pageSize page)

                nextPageNumber =
                    pageNumber page + 1
            in
            Just (pageRequest config nextPageNumber)


{-| Get page number
-}
pageNumber : Page a -> Int
pageNumber (Page page) =
    ceiling (toFloat page.startAt / toFloat page.maxResults) + 1


{-| Create a pagination configuration
-}
paginationConfig : Int -> PaginationConfig
paginationConfig itemsPerPage =
    PaginationConfig itemsPerPage


{-| Create a PageRequest for specific page number
-}
pageRequest : PaginationConfig -> Int -> PageRequest
pageRequest (PaginationConfig itemsPerPage) page =
    PageRequest itemsPerPage page


{-| Convert page request to query parameters that can be used to build a request URL
-}
pageRequestToQueryParams : PageRequest -> List QueryParameter
pageRequestToQueryParams (PageRequest itemsPerPage page) =
    [ Url.Builder.int "startAt" ((page - 1) * itemsPerPage)
    , Url.Builder.int "maxResults" itemsPerPage
    ]


{-| Extract a list of items from a page
-}
getItems : Page a -> List a
getItems (Page page) =
    page.values


{-| Page decoder. Requires decoder for paginated entity.
-}
pageDecoder : Decoder item -> Decoder (Page item)
pageDecoder itemDecoder =
    D.map5
        (\maxResults startAt total maybeIsLast values ->
            Page
                { maxResults = maxResults
                , startAt = startAt
                , total = total
                , isLast =
                    case maybeIsLast of
                        Just isLastValue ->
                            isLastValue

                        Nothing ->
                            startAt + List.length values < total
                , values = values
                }
        )
        (D.field "maxResults" D.int)
        (D.field "startAt" D.int)
        (D.field "total" D.int)
        (D.maybe (D.field "isLast" D.bool))
        (D.oneOf
            [ D.field "values" (D.list itemDecoder)
            , D.field "issues" (D.list itemDecoder)
            ]
        )


{-| Map page items
-}
map : (a -> b) -> Page a -> Page b
map f (Page page) =
    Page
        { maxResults = page.maxResults
        , startAt = page.startAt
        , total = page.total
        , isLast = page.isLast
        , values = List.map f page.values
        }
