module Warrior exposing
    ( Model, Msg
    , PlayerTurnFunction
    , Config, program
    , MultiplayerConfig, multiplayerProgram
    )

{-| Contains the essential logic for rendering and defining the game. The `Map` and `Player` modules will probably be more interesting.

@docs Model, Msg
@docs PlayerTurnFunction
@docs Config, program
@docs MultiplayerConfig, multiplayerProgram

-}

import Browser
import Color exposing (Color)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import List.Extra as List
import Palette.Cubehelix as Palette
import Process
import Task
import Warrior.Direction as Direction
import Warrior.History as History exposing (History)
import Warrior.Internal.Map as Map exposing (Map)
import Warrior.Item as Item
import Warrior.Map.Builder as MapTemplate
import Warrior.Player as Player exposing (Player)


{-| How would you like the game to be played? Which maps would you like to see the player try on, and how many milliseconds should we wait before showing the next turn?
-}
type alias Config =
    { maps : List MapTemplate.Builder
    , player : PlayerTurnFunction
    , msPerTurn : Float
    }


{-| Use this in your main function to start the game.
-}
program : Config -> Program () Model Msg
program config =
    Browser.element
        { init =
            always <|
                init
                    { maps = config.maps
                    , players = [ ( "Player", config.player ) ]
                    , msPerTurn = config.msPerTurn
                    , winCondition = doneWithCurrentMap
                    }
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


{-| Use this config to play with multiple warriors, and with a custom win condition.
-}
type alias MultiplayerConfig =
    { maps : List MapTemplate.Builder
    , players : List ( String, PlayerTurnFunction )
    , msPerTurn : Float
    , winCondition : List Player -> Map -> Bool
    }


{-| Start a program based of the multiplayer config
-}
multiplayerProgram : MultiplayerConfig -> Program () Model Msg
multiplayerProgram config =
    Browser.element
        { init = always <| init config
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


{-| The game model.
-}
type Model
    = Ongoing OngoingModel
    | Done (Maybe String)


type alias OngoingModel =
    { initialPlayers : List ( String, PlayerTurnFunction )
    , pcs : List PlayerDescription
    , currentMap : Map
    , remainingMaps : List MapTemplate.Builder
    , mapHistory : History
    , actionLog : List ( PlayerDescription, String )
    , winCondition : List Player -> Map -> Bool
    , updateInterval : Float
    }


{-| The type signature of an AI turn function
-}
type alias PlayerTurnFunction =
    Player -> Map -> History -> Player.Action


type alias PlayerDescription =
    { state : Player
    , turnFunction : PlayerTurnFunction
    , color : Color
    }


init : MultiplayerConfig -> ( Model, Cmd Msg )
init config =
    case config.maps of
        [] ->
            ( Done Nothing
            , Cmd.none
            )

        first :: rest ->
            ( modelWithMap first rest config.players config.winCondition config.msPerTurn
            , msgAfter 0 BeginRound
            )


modelWithMap :
    MapTemplate.Builder
    -> List MapTemplate.Builder
    -> List ( String, PlayerTurnFunction )
    -> (List Player -> Map -> Bool)
    -> Float
    -> Model
modelWithMap currentMap remainingMaps players winCondition updateInterval =
    let
        pcs =
            MapTemplate.spawnPoints currentMap
                |> List.map2 toPlayerDescription players

        toPlayerDescription ( id, turnFunc ) cord =
            { state = Player.spawnHero id cord
            , turnFunction = turnFunc
            , color = Color.fromRGB ( 0, 0, 0 )
            }

        npcs =
            MapTemplate.npcs currentMap
                |> List.map
                    (\( state, turnFunc ) ->
                        { state = state
                        , turnFunction = turnFunc
                        , color = Color.fromRGB ( 0, 0, 0 )
                        }
                    )

        playerDescriptions =
            List.append pcs npcs
                |> List.map2 (\clr pd -> { pd | color = clr }) playerColors

        playerColors =
            Palette.generate (List.length pcs + List.length npcs + 2)
                |> List.filter notBlackOrWhite

        notBlackOrWhite clr =
            let
                black =
                    ( 0, 0, 0 )

                white =
                    ( 255, 255, 255 )

                colorValues =
                    Color.toRGB clr
            in
            not (colorValues == black || colorValues == white)
    in
    Ongoing
        { initialPlayers = players
        , pcs = playerDescriptions
        , currentMap = MapTemplate.build currentMap
        , remainingMaps = remainingMaps
        , mapHistory = History.init
        , actionLog = []
        , winCondition = winCondition
        , updateInterval = updateInterval
        }


{-| The game message type.
-}
type Msg
    = InitializeMap (Maybe String)
    | BeginRound
    | TakeTurn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Done _ ->
            ( model, Cmd.none )

        Ongoing ongoingModel ->
            ongoingUpdate msg ongoingModel


ongoingUpdate : Msg -> OngoingModel -> ( Model, Cmd Msg )
ongoingUpdate msg model =
    case msg of
        InitializeMap maybeLastWinner ->
            case model.remainingMaps of
                [] ->
                    ( Done maybeLastWinner
                    , Cmd.none
                    )

                next :: rest ->
                    ( modelWithMap next rest model.initialPlayers model.winCondition model.updateInterval
                    , msgAfter 0 BeginRound
                    )

        BeginRound ->
            let
                possibleFirstLivingPlayer =
                    model.pcs
                        |> List.filter (.state >> Player.alive)
                        |> List.head
            in
            case possibleFirstLivingPlayer of
                Nothing ->
                    ( Done Nothing
                    , Cmd.none
                    )

                Just firstLivingPlayer ->
                    ( Ongoing model
                    , msgAfter 0 (TakeTurn (Player.id firstLivingPlayer.state))
                    )

        TakeTurn playerId ->
            case List.find (\pc -> Player.id pc.state == playerId) model.pcs of
                Nothing ->
                    -- Something wrong has happened, start from top of the turn order
                    ( Ongoing model
                    , msgAfter model.updateInterval BeginRound
                    )

                Just player ->
                    let
                        updatedModel =
                            playerTurn player model
                    in
                    ( Ongoing updatedModel
                    , if updatedModel.winCondition (List.map .state updatedModel.pcs) updatedModel.currentMap then
                        let
                            possibleSurvivingPlayer =
                                updatedModel.pcs
                                    |> List.filter (.state >> Player.alive)
                                    |> List.head
                                    |> Maybe.map (.state >> Player.id)
                        in
                        msgAfter model.updateInterval (InitializeMap possibleSurvivingPlayer)

                      else
                        model.pcs
                            |> List.dropWhile (\pc -> Player.id pc.state /= playerId)
                            |> List.drop 1
                            |> List.filter (.state >> Player.alive)
                            |> List.head
                            |> Maybe.map (.state >> Player.id >> TakeTurn)
                            |> Maybe.withDefault BeginRound
                            |> msgAfter model.updateInterval
                    )


msgAfter : Float -> Msg -> Cmd Msg
msgAfter updateInterval msg =
    Process.sleep updateInterval
        |> Task.perform (always msg)


playerTurn : PlayerDescription -> OngoingModel -> OngoingModel
playerTurn playerDescription model =
    let
        updatedMap =
            Map.setNpcs (List.map .state model.pcs) model.currentMap

        playerAction =
            playerDescription.turnFunction playerDescription.state updatedMap model.mapHistory

        updatePlayer fn event =
            { model
                | pcs =
                    List.map
                        (\desc ->
                            if desc == playerDescription then
                                { desc | state = fn desc.state }

                            else
                                desc
                        )
                        model.pcs
                , mapHistory =
                    History.record
                        playerDescription.state
                        updatedMap
                        playerAction
                        model.mapHistory
                , actionLog = ( playerDescription, event ) :: model.actionLog
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
                                    if desc.state == attackedPlayer then
                                        { desc | state = Player.attack playerDescription.state desc.state }

                                    else
                                        desc
                                )
                                model.pcs
                        , mapHistory =
                            History.record
                                playerDescription.state
                                updatedMap
                                playerAction
                                model.mapHistory
                        , actionLog =
                            ( playerDescription
                            , String.join " "
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
                            )
                                :: model.actionLog
                    }


doneWithCurrentMap : List Player -> Map -> Bool
doneWithCurrentMap players currentMap =
    List.any
        (Player.position >> Map.isExitPoint currentMap)
        players


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
                            |> List.filter (.state >> Player.alive)
                            |> List.map (\pd -> ( Player.position pd.state, pd.color ))
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

            Done winningPlayerId ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text <|
                        case winningPlayerId of
                            Just id ->
                                String.join " "
                                    [ "Congratulations"
                                    , id
                                    ]

                            Nothing ->
                                "Congratulations!"
                    )
        )


viewActionLog : ( PlayerDescription, String ) -> Element msg
viewActionLog ( pd, event ) =
    let
        ( r, g, b ) =
            Color.toRGB pd.color
    in
    Element.row
        [ Element.spacing 10 ]
        [ Element.el
            [ Element.width <| Element.px 10
            , Element.height <| Element.px 10
            , Border.rounded 50
            , Background.color <| Element.rgb255 (floor r) (floor g) (floor b)
            ]
            Element.none
        , Element.paragraph
            []
            [ Element.text (Player.id pd.state)
            , Element.text " "
            , Element.text event
            ]
        ]
