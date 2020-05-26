module Warrior exposing
    ( Config
    , Model
    , Msg
    , PlayerTurnFunction
    , program
    )

import Browser
import Element
import Html exposing (Html)
import Time
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Player)


type alias Config =
    { maps : List Map
    , player : PlayerTurnFunction
    , msPerTurn : Float
    }


program : Config -> Program () Model Msg
program config =
    Browser.element
        { init = always <| init config
        , update = update
        , view = view
        , subscriptions = subscriptions config.msPerTurn
        }


type Model
    = Ongoing OngoingModel
    | Done


type alias OngoingModel =
    { initialPlayers : List PlayerTurnFunction
    , pcs : List ( Player, PlayerTurnFunction )
    , currentMap : Map
    , remainingMaps : List Map
    }


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
        }


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

        updatePlayer fn =
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
            }
    in
    case playerAction of
        Player.Wait ->
            updatePlayer identity

        Player.Move dir ->
            let
                newCoordinate =
                    Map.coordinateFrom dir (Player.currentPosition player)
            in
            if Map.canMoveOnto newCoordinate updatedMap then
                updatePlayer (Player.withPosition newCoordinate)

            else
                model

        Player.Pickup ->
            let
                playerPos =
                    Player.currentPosition player

                maybeRemovedItem =
                    Map.removeItem playerPos model.currentMap
            in
            case maybeRemovedItem of
                Just ( removedItem, mapWithItemRemoved ) ->
                    let
                        modelWithUpdatedPlayer =
                            updatePlayer (Player.addItem removedItem)
                    in
                    { modelWithUpdatedPlayer | currentMap = mapWithItemRemoved }

                Nothing ->
                    updatePlayer identity

        Player.Heal ->
            updatePlayer Player.heal

        Player.Attack dir ->
            let
                attackCoordinate =
                    Map.coordinateFrom dir (Player.currentPosition player)

                possiblyAttackedPlayer =
                    List.map Tuple.first model.pcs
                        |> List.filter Player.alive
                        |> List.filter (\pc -> Player.currentPosition pc == attackCoordinate)
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
                    }


doneWithCurrentMap : OngoingModel -> Bool
doneWithCurrentMap state =
    List.any
        (Tuple.first
            >> Player.currentPosition
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
                            |> List.map Player.currentPosition
                in
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Map.view playerPositions state.currentMap)

            Done ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Congratulations!")
        )


subscriptions : Float -> Model -> Sub Msg
subscriptions msPerTurn model =
    case model of
        Ongoing _ ->
            Time.every msPerTurn (always Step)

        Done ->
            Sub.none
