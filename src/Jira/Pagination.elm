module Jira.Pagination exposing
    ( Page
    , PageRequest
    , PaginationConfig
    , getItems
    , isLast
    , nextPage
    , pageDecoder
    , pageNumber
    , pageRequest
    , pageRequestToQueryParams
    , paginationConfig
    , totalPages
    )

import Json.Decode as D exposing (Decoder)
import Url.Builder exposing (QueryParameter)


type PageRequest
    = PageRequest Int Int


type PaginationConfig
    = PaginationConfig Int


type alias PageInternals item =
    { self : String
    , maxResults : Int
    , startAt : Int
    , total : Int
    , isLast : Bool
    , values : List item
    }


type Page item
    = Page (PageInternals item)


isLast : Page a -> Bool
isLast (Page page) =
    page.isLast


totalPages : Page a -> Int
totalPages (Page page) =
    ceiling (toFloat page.total / toFloat page.maxResults)


pageSize : Page a -> Int
pageSize (Page page) =
    page.maxResults


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


pageNumber : Page a -> Int
pageNumber (Page page) =
    ceiling (toFloat page.startAt / toFloat page.maxResults) + 1


paginationConfig : Int -> PaginationConfig
paginationConfig itemsPerPage =
    PaginationConfig itemsPerPage


pageRequest : PaginationConfig -> Int -> PageRequest
pageRequest (PaginationConfig itemsPerPage) page =
    PageRequest itemsPerPage page


pageRequestToQueryParams : PageRequest -> List QueryParameter
pageRequestToQueryParams (PageRequest itemsPerPage page) =
    [ Url.Builder.int "startAt" ((page - 1) * itemsPerPage)
    , Url.Builder.int "maxResults" itemsPerPage
    ]


getItems : Page a -> List a
getItems (Page page) =
    page.values


pageDecoder : Decoder item -> Decoder (Page item)
pageDecoder itemDecoder =
    D.map Page
        (D.map6 PageInternals
            (D.field "self" D.string)
            (D.field "maxResults" D.int)
            (D.field "startAt" D.int)
            (D.field "total" D.int)
            (D.field "isLast" D.bool)
            (D.field "values" (D.list itemDecoder))
        )
