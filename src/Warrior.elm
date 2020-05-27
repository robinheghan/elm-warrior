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
    { initialPlayers : List PlayerTurnFunction
    , pcs : List ( Player, PlayerTurnFunction )
    , currentMap : Map
    , remainingMaps : List Map
    , actionLog : List String
    }


{-| The type signature of an AI turn function
-}
type alias PlayerTurnFunction =
    Player -> Map -> Player.Action


init : Config -> ( Model, Cmd msg )
init config =
    case config.maps of
        [] ->
            ( Done
            , Cmd.none
            )

        first :: rest ->
            ( modelWithMap [ config.player ] first rest
            , Cmd.none
            )


modelWithMap : List PlayerTurnFunction -> Map -> List Map -> Model
modelWithMap players currentMap remainingMaps =
    let
        pcs =
            Map.spawnPoints currentMap
                |> List.map Player.spawnHero
                |> (\spawnedPlayers -> List.map2 Tuple.pair spawnedPlayers players)
                |> List.append (Map.npcs currentMap)
    in
    Ongoing
        { initialPlayers = players
        , pcs = pcs
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
                    |> List.filter (\( pc, _ ) -> Player.alive pc)
                    |> List.foldl playerTurn state
                    |> Ongoing
                , Cmd.none
                )


playerTurn : ( Player, PlayerTurnFunction ) -> OngoingModel -> OngoingModel
playerTurn ( player, turnFn ) model =
    let
        updatedMap =
            Map.setNpcs model.pcs model.currentMap

        playerAction =
            turnFn player updatedMap

        updatePlayer fn event =
            { model
                | pcs =
                    List.map
                        (\( pc, turnFunc ) ->
                            if pc == player then
                                ( pc
                                    |> Player.addAction playerAction
                                    |> fn
                                , turnFunc
                                )

                            else
                                ( pc, turnFunc )
                        )
                        model.pcs
                , actionLog = event :: model.actionLog
            }
    in
    case playerAction of
        Player.Wait ->
            updatePlayer identity "Player waits"

        Player.Move dir ->
            let
                newCoordinate =
                    Map.coordinateFrom dir (Player.position player)
            in
            if Map.canMoveOnto newCoordinate updatedMap then
                updatePlayer (Player.withPosition newCoordinate) <|
                    String.join " "
                        [ "Player moves"
                        , dir
                            |> Direction.toString
                            |> String.toLower
                        ]

            else
                updatePlayer identity "Player tries to move to an impossible position!"

        Player.Pickup ->
            let
                playerPos =
                    Player.position player

                maybeRemovedItem =
                    Map.removeItem playerPos model.currentMap
            in
            case maybeRemovedItem of
                Just ( removedItem, mapWithItemRemoved ) ->
                    let
                        modelWithUpdatedPlayer =
                            updatePlayer (Player.addItem removedItem) <|
                                String.join " "
                                    [ "Player picked up"
                                    , removedItem
                                        |> Item.toString
                                        |> String.toLower
                                    ]
                    in
                    { modelWithUpdatedPlayer | currentMap = mapWithItemRemoved }

                Nothing ->
                    updatePlayer identity <|
                        "Player tried to pick up an item, but there is no item to pick up."

        Player.Heal ->
            updatePlayer Player.heal <|
                "Player takes a rest, improving their strength."

        Player.Attack dir ->
            let
                attackCoordinate =
                    Map.coordinateFrom dir (Player.position player)

                possiblyAttackedPlayer =
                    List.map Tuple.first model.pcs
                        |> List.filter Player.alive
                        |> List.filter (\pc -> Player.position pc == attackCoordinate)
                        |> List.head
            in
            case possiblyAttackedPlayer of
                Nothing ->
                    model

                Just attackedPlayer ->
                    { model
                        | pcs =
                            List.map
                                (\( pc, turnFunc ) ->
                                    if pc == player then
                                        ( Player.addAction playerAction pc
                                        , turnFunc
                                        )

                                    else if pc == attackedPlayer then
                                        ( Player.attack player pc
                                        , turnFunc
                                        )

                                    else
                                        ( pc, turnFunc )
                                )
                                model.pcs
                        , actionLog =
                            String.join " "
                                [ "Player attacks"
                                , dir
                                    |> Direction.toString
                                    |> String.toLower
                                    |> String.append "."
                                , "Dealing"
                                , Player.attackDamage player
                                    |> String.fromInt
                                , "damage."
                                ]
                                :: model.actionLog
                    }


doneWithCurrentMap : OngoingModel -> Bool
doneWithCurrentMap state =
    List.any
        (Tuple.first
            >> Player.position
            >> Map.isExitPoint state.currentMap
        )
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
                            |> List.map Tuple.first
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
