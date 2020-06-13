module Warrior.Program exposing
    ( TurnFunction
    , Model, Msg
    , Config, program
    )

{-| Contains the essential logic for rendering and defining the game. The `Map` and `Player` modules will be of more interest for programming your warrior.

@docs TurnFunction
@docs Model, Msg
@docs Config, program

-}

import Browser
import Color exposing (Color)
import Dict
import Dict.Extra as Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import List.Extra as List
import Palette.Cubehelix as Palette
import Process
import Set
import Task
import Warrior exposing (Warrior)
import Warrior.Direction as Direction
import Warrior.Internal.History as History exposing (History)
import Warrior.Internal.Map as Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item
import Warrior.Map.Builder as MapTemplate exposing (Template)
import Warrior.Map.Progression as Progression exposing (Progression, ProgressionFunction)
import Warrior.Map.Tile as Tile


{-| The type signature of a turn function. Turn functions with decide the actions of a Warrior.
-}
type alias TurnFunction =
    Warrior -> Map -> History -> Warrior.Action


{-| This configuration decides what kind of game you'll play.
You can find a bunch of pre-made maps in the `Warrior.Maps` module, unless you want to make your own.
For players you can add a list of ( PlayerName, TurnFunction ).
You can decide how long to wait between turns using the msPerTurn field.
Finally, the progressionFunction decides when the game is over, or when it is won. You can find some progressionFunctions in `Warrior.Maps.Progression`.
-}
type alias Config =
    { maps : List Template
    , players : List ( String, TurnFunction )
    , msPerTurn : Float
    , progressionFunction : ProgressionFunction
    }


{-| Start a program with a given config.
-}
program : Config -> Program () Model Msg
program config =
    Browser.element
        { init = always <| init config
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Config -> ( Model, Cmd Msg )
init config =
    case config.maps of
        [] ->
            ( Done Nothing
            , Cmd.none
            )

        first :: rest ->
            ( modelWithMap first rest config.players config.progressionFunction config.msPerTurn
            , msgAfter 0 BeginRound
            )



-- UPDATE


{-| The game model.
-}
type Model
    = Ongoing OngoingModel
    | Done (Maybe (List Warrior))


type alias OngoingModel =
    { warriors : List PlayerDescription
    , currentMap : Map
    , remainingMaps : List Template
    , mapHistory : History
    , actionLog : List ( String, String )
    , progressionFunction : ProgressionFunction
    , updateInterval : Float
    }


type alias PlayerDescription =
    { state : Warrior
    , turnFunction : TurnFunction
    , color : Color
    }


{-| The game message type.
-}
type Msg
    = InitializeMap Progression
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
        InitializeMap progression ->
            case ( progression, model.remainingMaps ) of
                ( Progression.Advance players, [] ) ->
                    ( Done (Just players)
                    , Cmd.none
                    )

                ( Progression.Advance players, next :: rest ) ->
                    let
                        advancingPlayerIds =
                            List.map Player.id players
                                |> Set.fromList

                        advancingPlayers =
                            model.warriors
                                |> List.filter (\pc -> Set.member (Player.id pc.state) advancingPlayerIds)
                                |> List.map (\pc -> ( Player.id pc.state, pc.turnFunction ))
                    in
                    ( modelWithMap next rest advancingPlayers model.progressionFunction model.updateInterval
                    , msgAfter 0 BeginRound
                    )

                _ ->
                    ( Done Nothing
                    , Cmd.none
                    )

        BeginRound ->
            let
                possibleFirstLivingPlayer =
                    List.find (\pc -> Player.isHero pc.state && Player.alive pc.state) model.warriors
            in
            case possibleFirstLivingPlayer of
                Nothing ->
                    ( Done Nothing
                    , Cmd.none
                    )

                Just firstLivingPlayer ->
                    ( Ongoing model
                    , msgAfter model.updateInterval (TakeTurn (Player.id firstLivingPlayer.state))
                    )

        TakeTurn playerId ->
            case List.find (\pc -> Player.id pc.state == playerId) model.warriors of
                Nothing ->
                    -- Something wrong has happened, start from top of the turn order
                    ( Ongoing model
                    , msgAfter 0 BeginRound
                    )

                Just player ->
                    let
                        updatedModel =
                            playerTurn player model

                        players =
                            updatedModel.warriors
                                |> List.map .state
                                |> List.filter Player.isHero
                    in
                    ( Ongoing updatedModel
                    , case updatedModel.progressionFunction players updatedModel.currentMap updatedModel.mapHistory of
                        Progression.Undecided ->
                            model.warriors
                                |> List.dropWhile (\pc -> Player.id pc.state /= playerId)
                                |> List.drop 1
                                |> List.find (.state >> Player.alive)
                                |> Maybe.map (.state >> Player.id >> TakeTurn >> msgAfter model.updateInterval)
                                |> Maybe.withDefault (msgAfter 0 BeginRound)

                        other ->
                            msgAfter model.updateInterval (InitializeMap other)
                    )


modelWithMap :
    Template
    -> List Template
    -> List ( String, TurnFunction )
    -> ProgressionFunction
    -> Float
    -> Model
modelWithMap currentMap remainingMaps players progressionFunction updateInterval =
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

        playerIdDict =
            Dict.fromListBy (.state >> Player.id) playerDescriptions

        playersWithUniqueIds =
            List.filter uniquePlayer playerDescriptions

        uniquePlayer pc =
            case Dict.get (Player.id pc.state) playerIdDict of
                Nothing ->
                    False

                Just pcInDict ->
                    pcInDict == pc
    in
    Ongoing
        { warriors = playersWithUniqueIds
        , currentMap = MapTemplate.build currentMap
        , remainingMaps = remainingMaps
        , mapHistory = History.init
        , actionLog = []
        , progressionFunction = progressionFunction
        , updateInterval = updateInterval
        }


msgAfter : Float -> Msg -> Cmd Msg
msgAfter updateInterval msg =
    Process.sleep updateInterval
        |> Task.perform (always msg)


playerTurn : PlayerDescription -> OngoingModel -> OngoingModel
playerTurn playerDescription model =
    let
        updatedMap =
            Map.setNpcs (List.map .state model.warriors) model.currentMap

        playerAction =
            playerDescription.turnFunction playerDescription.state updatedMap model.mapHistory

        updatePlayer fn event =
            { model
                | warriors =
                    List.map
                        (\desc ->
                            if desc == playerDescription then
                                { desc | state = fn desc.state }

                            else
                                desc
                        )
                        model.warriors
                , mapHistory =
                    History.record
                        playerDescription.state
                        updatedMap
                        playerAction
                        model.mapHistory
                , actionLog = ( Player.id playerDescription.state, event ) :: model.actionLog
            }
    in
    case playerAction of
        Warrior.Wait ->
            updatePlayer identity "waits"

        Warrior.Move dir ->
            let
                targetCoordinate =
                    Player.position playerDescription.state
                        |> Map.coordinateFrom dir

                canMove =
                    Map.tileAtPosition targetCoordinate updatedMap
                        |> Tile.canMoveOnto
            in
            if canMove then
                updatePlayer (Player.withPosition targetCoordinate) <|
                    String.join " "
                        [ "moves"
                        , dir
                            |> Direction.toString
                            |> String.toLower
                        ]

            else
                updatePlayer identity "tries to move to an impossible position!"

        Warrior.Pickup ->
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

        Warrior.Heal ->
            updatePlayer Player.heal <|
                String.join " "
                    [ "takes a rest, improving their strength by"
                    , String.fromInt <| Player.healingPotential playerDescription.state
                    , "points."
                    ]

        Warrior.Attack dir ->
            let
                attackCoordinate =
                    Map.coordinateFrom dir (Player.position playerDescription.state)

                possiblyAttackedPlayer =
                    List.map .state model.warriors
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
                        | warriors =
                            List.map
                                (\desc ->
                                    if desc.state == attackedPlayer then
                                        { desc | state = Player.attack playerDescription.state desc.state }

                                    else
                                        desc
                                )
                                model.warriors
                        , mapHistory =
                            History.record
                                playerDescription.state
                                updatedMap
                                playerAction
                                model.mapHistory
                        , actionLog =
                            ( Player.id playerDescription.state
                            , String.join " "
                                [ "attacks"
                                , dir
                                    |> Direction.toString
                                    |> String.toLower
                                    |> (\s -> String.append s ".")
                                , "Dealing"
                                , Player.attackDamage playerDescription.state
                                    |> String.fromInt
                                , "damage."
                                ]
                            )
                                :: model.actionLog
                    }



-- VIEW


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
                        state.warriors
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

            Done possibleWinners ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.text <|
                        case possibleWinners of
                            Just [] ->
                                "Congratulations!"

                            Just winners ->
                                String.join ""
                                    [ "Congratulations"
                                    , String.join ", " <|
                                        List.map Player.id winners
                                    ]

                            Nothing ->
                                "Game Over"
                    )
        )


viewActionLog : ( String, String ) -> Element msg
viewActionLog ( playerId, event ) =
    Element.paragraph
        []
        [ Element.text playerId
        , Element.text " "
        , Element.text event
        ]
