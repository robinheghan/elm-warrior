module Warrior exposing
    ( Model, Msg
    , PlayerTurnFunction
    , Config, program
    )

{-| Contains the essential logic for rendering and defining the game. The `Map` and `Player` modules will probably be more interesting.

@docs Model, Msg
@docs PlayerTurnFunction
@docs Config, program

-}

import Browser
import Element exposing (Element)
import Element.Font as Font
import Html exposing (Html)
import Time
import Warrior.Direction as Direction
import Warrior.Item as Item
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Player)


{-| How would you like the game to be played? Which maps would you like to see the player try on, and how many milliseconds should we wait before showing the next turn?
-}
type alias Config =
    { maps : List Map
    , player : PlayerTurnFunction
    , msPerTurn : Float
    }


{-| Use this in your main function to start the game.
-}
program : Config -> Program () Model Msg
program config =
    Browser.element
        { init = always <| init config
        , update = update
        , view = view
        , subscriptions = subscriptions config.msPerTurn
        }


{-| The game model.
-}
type Model
    = Ongoing OngoingModel
    | Done


type alias OngoingModel =
    { initialPlayers : List ( String, PlayerTurnFunction )
    , pcs : List PlayerDescription
    , currentMap : Map
    , remainingMaps : List Map
    , actionLog : List String
    }


{-| The type signature of an AI turn function
-}
type alias PlayerTurnFunction =
    Player -> Map -> Player.Action


type alias PlayerDescription =
    { id : String
    , state : Player
    , turnFunction : PlayerTurnFunction
    }


init : Config -> ( Model, Cmd msg )
init config =
    case config.maps of
        [] ->
            ( Done
            , Cmd.none
            )

        first :: rest ->
            ( modelWithMap [ ( "Player", config.player ) ] first rest
            , Cmd.none
            )


modelWithMap : List ( String, PlayerTurnFunction ) -> Map -> List Map -> Model
modelWithMap players currentMap remainingMaps =
    let
        pcs =
            Map.spawnPoints currentMap
                |> List.map Player.spawnHero
                |> List.map2 toPlayerDescription players

        npcs =
            Map.npcs currentMap
                |> List.indexedMap
                    (\idx ( state, turnFunc ) ->
                        toPlayerDescription ( "NPC " ++ String.fromInt idx, turnFunc ) state
                    )

        toPlayerDescription ( id, turnFunc ) state =
            { id = id
            , state = state
            , turnFunction = turnFunc
            }
    in
    Ongoing
        { initialPlayers = players
        , pcs = List.append pcs npcs
        , currentMap = currentMap
        , remainingMaps = remainingMaps
        , actionLog = []
        }


{-| The game message type.
-}
type Msg
    = Step


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    case model of
        Done ->
            ( model, Cmd.none )

        Ongoing state ->
            if doneWithCurrentMap state then
                case state.remainingMaps of
                    [] ->
                        ( Done
                        , Cmd.none
                        )

                    next :: rest ->
                        ( modelWithMap state.initialPlayers next rest
                        , Cmd.none
                        )

            else
                ( state.pcs
                    |> List.filter (.state >> Player.alive)
                    |> List.foldl playerTurn state
                    |> Ongoing
                , Cmd.none
                )


playerTurn : PlayerDescription -> OngoingModel -> OngoingModel
playerTurn playerDescription model =
    let
        updatedMap =
            Map.setNpcs (List.map (\desc -> ( desc.state, desc.turnFunction )) model.pcs) model.currentMap

        playerAction =
            playerDescription.turnFunction playerDescription.state updatedMap

        updatePlayer fn event =
            { model
                | pcs =
                    List.map
                        (\desc ->
                            if desc == playerDescription then
                                { desc
                                    | state =
                                        desc.state
                                            |> Player.addAction playerAction
                                            |> fn
                                }

                            else
                                desc
                        )
                        model.pcs
                , actionLog = String.join " " [ playerDescription.id, event ] :: model.actionLog
            }
    in
    case playerAction of
        Player.Wait ->
            updatePlayer identity "waits"

        Player.Move dir ->
            let
                newCoordinate =
                    Map.coordinateFrom dir (Player.position playerDescription.state)
            in
            if Map.canMoveOnto newCoordinate updatedMap then
                updatePlayer (Player.withPosition newCoordinate) <|
                    String.join " "
                        [ "moves"
                        , dir
                            |> Direction.toString
                            |> String.toLower
                        ]

            else
                updatePlayer identity "tries to move to an impossible position!"

        Player.Pickup ->
            let
                playerPos =
                    Player.position playerDescription.state

                maybeRemovedItem =
                    Map.removeItem playerPos model.currentMap
            in
            case maybeRemovedItem of
                Just ( removedItem, mapWithItemRemoved ) ->
                    let
                        modelWithUpdatedPlayer =
                            updatePlayer (Player.addItem removedItem) <|
                                String.join " "
                                    [ "picked up"
                                    , removedItem
                                        |> Item.toString
                                        |> String.toLower
                                    ]
                    in
                    { modelWithUpdatedPlayer | currentMap = mapWithItemRemoved }

                Nothing ->
                    updatePlayer identity <|
                        "tried to pick up an item, but there is no item to pick up."

        Player.Heal ->
            updatePlayer Player.heal <|
                String.join " "
                    [ "takes a rest, improving their strength by"
                    , String.fromInt <| Player.healingPotential playerDescription.state
                    , "points."
                    ]

        Player.Attack dir ->
            let
                attackCoordinate =
                    Map.coordinateFrom dir (Player.position playerDescription.state)

                possiblyAttackedPlayer =
                    List.map .state model.pcs
                        |> List.filter Player.alive
                        |> List.filter (\pc -> Player.position pc == attackCoordinate)
                        |> List.head
            in
            case possiblyAttackedPlayer of
                Nothing ->
                    updatePlayer identity <|
                        "tried attacking, but no one was there."

                Just attackedPlayer ->
                    { model
                        | pcs =
                            List.map
                                (\desc ->
                                    if desc.state == playerDescription.state then
                                        { desc | state = Player.addAction playerAction desc.state }

                                    else if desc.state == attackedPlayer then
                                        { desc | state = Player.attack playerDescription.state desc.state }

                                    else
                                        desc
                                )
                                model.pcs
                        , actionLog =
                            String.join " "
                                [ "attacks"
                                , dir
                                    |> Direction.toString
                                    |> String.toLower
                                    |> String.append "."
                                , "Dealing"
                                , Player.attackDamage playerDescription.state
                                    |> String.fromInt
                                , "damage."
                                ]
                                :: model.actionLog
                    }


doneWithCurrentMap : OngoingModel -> Bool
doneWithCurrentMap state =
    List.any
        (.state >> Player.position >> Map.isExitPoint state.currentMap)
        state.pcs


view : Model -> Html msg
view model =
    Element.layout
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        (case model of
            Ongoing state ->
                let
                    playerPositions =
                        state.pcs
                            |> List.map .state
                            |> List.filter Player.alive
                            |> List.map Player.position
                in
                Element.column
                    [ Element.centerX
                    , Element.centerY
                    ]
                    [ Element.el
                        [ Element.centerX
                        , Element.paddingEach
                            { top = 0
                            , left = 0
                            , right = 0
                            , bottom = 50
                            }
                        ]
                        (Map.view playerPositions state.currentMap)
                    , Element.column
                        [ Element.width Element.fill
                        , Element.height <| Element.px 100
                        , Element.clip
                        , Element.scrollbarY
                        ]
                        (List.map viewActionLog state.actionLog)
                    ]

            Done ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Congratulations!")
        )


viewActionLog : String -> Element msg
viewActionLog event =
    Element.paragraph
        [ Font.center ]
        [ Element.text event ]


subscriptions : Float -> Model -> Sub Msg
subscriptions msPerTurn model =
    case model of
        Ongoing _ ->
            Time.every msPerTurn (always Step)

        Done ->
            Sub.none
