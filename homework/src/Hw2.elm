module Hw2 exposing (..)

import Maybe exposing (withDefault)
import Url.Builder exposing (absolute, string, int)

--------------------------------------------------------------------------------
-- Helpers

-- Maybe -> String
listMaybeToString
  : List { name : Maybe String, email : Maybe String}
  -> List { name : String, email : String}
listMaybeToString list =
  List.map (\rec -> { name = withDefault "" rec.name, email = withDefault "" rec.email }) list

--------------------------------------------------------------------------------
-- Map one structure to another
convert
  : List { name : String, email : String, phone_number : String}
  -> List { name : String, email : String}

convert list =
  List.map (\rec -> { name = rec.name, email = rec.email }) list

-- convert [{name="John", email="john@gmail.com", phone_number="+3801234567"}]
--[{name="John", email="john@gmail.com"}]

--------------------------------------------------------------------------------
-- Filter elements with non-empty name and email
convert02
  : List { name : Maybe String, email : Maybe String}
  -> List { name : String, email : String}

convert02 list =
  let
    filteredList = 
      (List.filter
        (\rec -> not (String.isEmpty (withDefault "" rec.name) || String.isEmpty (withDefault "" rec.email)))
        list
      )
  in
  listMaybeToString filteredList

-------------------------------
andMap =
  Maybe.map2 (|>)

convert02Applicative xs =
  catMaybes
    (List.map
      (\x ->
        Just (\n e -> { name = n, email = e })
          |> andMap x.name
          |> andMap x.email
      )
      xs
    )
-- andMap : Maybe a -> Maybe (a -> b) -> Maybe b
-- b may be a function (b -> c) itself:
-- andMap : Maybe a -> Maybe (a -> (b -> c)) -> Maybe (b -> c)
-- Therefore, we can chain andMap to bigger number of fields:
--  Just (\a b c d e ... -> {a = a, ...}) |> andMap x.a |> ...
-------------------------------

-- convert02 [{name=Just "John", email=Just "john@gmail.com"}]
--[{name="John", email="john@gmail.com"}]

--------------------------------------------------------------------------------
-- Fill in missing emails with <unspecified>, while removing elements with no name
convert03
  : List { name : Maybe String, email : Maybe String}
  -> List { name : String, email : String}

convert03 list =
  let
    normalizeEmail email =
      case email of
        Nothing ->
          "<unspecified>"
        Just str ->
          str

    listWithNames = List.filter (\rec -> not (String.isEmpty (withDefault "" rec.name))) list
  in
  List.map (\rec -> { name = (withDefault "" rec.name), email = (normalizeEmail rec.email) }) listWithNames

-- convert03 [{name=Just "John", email=Nothing}]
--[{name="John", email="<unspecified>"}]

--------------------------------------------------------------------------------
-- Rewrite bird using <|, then using |> instead of parens (where applicable)

bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))

-- using <|
bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr <| [ 1, 2, 3 ]

-- using |>
bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
      |> List.map incr
      |> List.filter notThree
      |> List.sum

--------------------------------------------------------------------------------
-- Implement setPhone

type alias User = { profile : Profile }
type alias Profile = { address : Address }
type alias Address = { phone : String }

setPhone : String -> User -> User
setPhone phone user =
  let
    setNewPhone addr phoneNew =
      { addr | phone = phoneNew }
    setAddr profile addr =
      { profile | address = addr }
    setProfile user2 profile =
      { user2 | profile = profile }
  in
  phone
    |> setNewPhone user.profile.address
    |> setAddr user.profile
    |> setProfile user

-- setPhone "+123456" { profile = { address = { phone = "+654321" } } }
--{ profile = { address = { phone = "+123456" } } }

--------------------------------------------------------------------------------
-- mapMaybes

mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes func list =
  let
    mappedList = List.map func list

    droppedNothingList = catMaybes mappedList
  in
  droppedNothingList

-- mapMaybes (\x -> if x == Just 3 then x else Just 4) [Just 1, Nothing, Just 3]
--[4,4,3] : List number

--------------------------------------------------------------------------------
-- catMaybes

catMaybes : List (Maybe a) -> List a
catMaybes list =
  case list of
    [] ->
      []
    Nothing :: tail ->
      catMaybes tail
    Just x :: tail ->
      [ x ] ++ catMaybes tail

-- catMaybes [Just 1, Nothing, Just 3]
--[1,3] : List number

--------------------------------------------------------------------------------
--buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
--buildStatsUrl itemId ps =
--  "https://myapi.com/api/item/" ++
--  absolute [ String.fromInt itemId, "stats.json" ]
--      (Maybe.map2 (\s n -> [ string "start_date" s, int "num_items" n ]) ps.startDate ps.numElems)

-- > buildStatsUrl 12 {startDate=Nothing, numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json
-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01
-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Just 10}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10
