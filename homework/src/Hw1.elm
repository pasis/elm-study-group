module Hw1 exposing (..)

-- Find last element in a list
myLast list =
  let
    length = List.length list
  in
  if length == 0 then
    Nothing
  else
    List.head (List.drop (length - 1) list)

-- Find the last but one element of a list
myButLast list =
  let
    length = List.length list
  in
  if length < 2 then
    Nothing
  else
    List.head (List.drop (length - 2) list)

-- Find the K'th element of a list
elementAt list pos =
  let
    length = List.length list
  in
  if pos > length then
    Nothing
  else if pos < 1 then
    Nothing
  else
    List.head (List.drop (pos - 1) list)

-- Find the number of elements of a list
myLength list =
  let
    myLengthHelper tmpList counter =
      if tmpList == [] then
        counter
      else
        myLengthHelper (List.drop 1 tmpList) (counter + 1)
  in
  myLengthHelper list 0

-- Reverse a list
myReverse2 list =
  let
    myReverseHelper tmpList reverseList =
      if tmpList == [] then
        reverseList
      else
        myReverseHelper (List.drop 1 tmpList) ([List.head tmpList] ++ reverseList)
  in
  myReverseHelper list []

myReverse list =
  case list of
    [] ->
      []
    y :: tail ->
      myReverse tail ++ [y]

-- Find out whether a list is a palindrome
isPalindrome list = list == (myReverse list)

-- Eliminate consecutive duplicates of string elements
compress str =
  let
    myListTail list =
      List.drop 1 list

    myConcat s c =
      s ++ (String.fromChar (Maybe.withDefault '0' c))

    compressHelper tmpList lastChar result =
      if (List.isEmpty tmpList) then
        result
      else if (List.head tmpList) == lastChar then
        compressHelper (myListTail tmpList) lastChar result
      else
        compressHelper (myListTail tmpList) (List.head tmpList) (myConcat result (List.head tmpList))
  in
  compressHelper (String.toList str) Nothing ""

-- Drop every N'th element from a string
dropEvery str nth =
  let
    myListTail list =
      List.drop 1 list

    myConcat s c =
      s ++ (String.fromChar (Maybe.withDefault '0' c))

    dropEveryHelper tmpList result counter n =
      if List.isEmpty tmpList then
        result
      else if counter == n then
        dropEveryHelper (myListTail tmpList) result 1 n
      else
        dropEveryHelper (myListTail tmpList) (myConcat result (List.head tmpList)) (counter + 1) n
  in
  if nth < 1 then
    ""
  else
    dropEveryHelper (String.toList str) "" 1 nth

-- (optional) Insert the emoji between words
clap str =
  let
    myListTail list =
      List.drop 1 list

    appendWord result origin start end =
      if end - start < 2 then
        result
      else if result == "" then
        String.slice start end origin
      else
        result ++ " " ++ "👏" ++ (String.slice start end origin)

    clapHelper idxList lastPos origin result =
      if List.isEmpty idxList then
        appendWord result origin lastPos (String.length origin)
      else
        clapHelper (myListTail idxList) (Maybe.withDefault 0 (List.head idxList)) origin (appendWord result origin lastPos (Maybe.withDefault 0 (List.head idxList)))
  in
  clapHelper (String.indexes " " str) 0 str ""

