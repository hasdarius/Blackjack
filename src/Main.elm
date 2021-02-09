-----------------------
-- Darius Has
-- 12.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main,calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Debug



import Card exposing (..)
import List exposing(..)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | Retry
  | NewCard Card
  | ToogleDeck


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Retry -> 
      (startingModel
      ,Cmd.none
      )
    Draw ->
      ( model
      , drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( {hand=newCard::model.hand
      ,deck=List.filter (\x -> x /= newCard) model.deck
      ,showDeck=model.showDeck
      }
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
        {hand=model.hand
      ,deck= model.deck
      ,showDeck= not (model.showDeck)
      }
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
calculateScore : List Card -> Int
calculateScore cards = 
    let
          generateAllLists : List (List Int) -> List(List Int) -> List (List Int)
          generateAllLists l acc=
            case l of
            [] -> acc
            x::xs -> case x of
                          [value1,value2]-> (generateAllLists xs ( List.map (\myList->value1::myList) acc)) ++ ( generateAllLists xs  ( List.map(\myList->value2::myList) acc) )
                          [value3] -> generateAllLists xs (List.map (\myList->value3::myList) acc)
                          _ -> generateAllLists xs acc
        
          getAllScores: List(List Int) -> List Int -> List Int     
          getAllScores l acc=
                  case l of
                    [] ->acc 
                    x::xs -> (getAllScores xs (List.sum(x)::acc))

          allSums= List.sort(getAllScores (generateAllLists (List.map cardValue cards) [[]] ) [])
                
          getMinimumScore : List Int -> Int      
          getMinimumScore l =
                  case (filter (\x -> x <= 21) l) of
                        [] -> List.sum(take 1 l)
                        myList  ->List.sum(take 1 (List.reverse myList))
    in
    getMinimumScore allSums
  


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
  in
    div []
      [ div [] [ h1 [] [text appName] ],
      button [ onClick Draw ] [ text "Draw a Card" ],
      button [ onClick ToogleDeck ] [ text "Toggle Deck Visibility" ],
      div[] [h6 [] (List.map(\x -> viewCard x) model.hand) ],
      div[] [h6 [] [text "Remaining deck:"]],
      div[] [h6 [] (if(model.showDeck)  then(List.map(\x -> viewCard x) model.deck) else [text "Deck not visible"])],
      div[] [h4 [] [text ("Your score: "++String.fromInt(calculateScore model.hand))]],
      div[] [h4 [] [case calculateScore model.hand of
                    21-> text "You won!"
                    score-> if(score>21) then text "You lost!" else text ""]],
      button [ onClick Retry] [text "Play Again!"]
      ]