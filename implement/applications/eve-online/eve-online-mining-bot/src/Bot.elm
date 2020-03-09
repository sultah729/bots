{- EVE Online mining bot version 2020-02-15 - 2020-03-09 TheRealManiac - point 2

   The bot warps to an asteroid belt, mines there until the ore hold is full, and then docks at a station to unload the ore. It then repeats this cycle until you stop it.
   It remembers the station in which it was last docked, and docks again at the same station.

   Setup instructions for the EVE Online client:
   + Enable `Run clients with 64 bit` in the settings, because this bot only works with the 64-bit version of the EVE Online client.
   + Set the UI language to English.
   + In Overview window, make asteroids visible.
   + Set the Overview window to sort objects in space by distance with the nearest entry at the top.
   + In the Inventory window select the 'List' view.
   + Setup inventory window so that 'Ore Hold' is always selected.
   + In the ship UI, arrange the mining modules to appear all in the upper row of modules.
   + Enable the info panel 'System info'.
-}
{-
   bot-catalog-tags:eve-online,mining
   authors-forum-usernames:viir
-}


module Bot exposing
    ( State
    , initState
    , processEvent
    )

import BotEngine.Interface_To_Host_20200213 as InterfaceToHost
import EveOnline.BotFramework exposing (BotEffect(..), getEntropyIntFromUserInterface)
import EveOnline.MemoryReading
    exposing
        ( MaybeVisible(..)
        , OverviewWindow
        , OverviewWindowEntry
        , ParsedUserInterface
        , ShipUI
        , ShipUIModule
        , centerFromDisplayRegion
        , maybeNothingFromCanNotSeeIt
        , maybeVisibleAndThen
        )
import EveOnline.VolatileHostInterface as VolatileHostInterface exposing (MouseButton(..), effectMouseClickAtLocation)
import Set


runAwayShieldHitpointsThresholdPercent : Int
runAwayShieldHitpointsThresholdPercent =
    50


enterCombatShieldHitpointsThresholdPercent : Int
enterCombatShieldHitpointsThresholdPercent =
    99


type alias UIElement =
    EveOnline.MemoryReading.UITreeNodeWithDisplayRegion


type alias TreeLeafAct =
    { firstAction : VolatileHostInterface.EffectOnWindowStructure
    , followingSteps : List ( String, ParsedUserInterface -> Maybe VolatileHostInterface.EffectOnWindowStructure )
    }


type EndDecisionPathStructure
    = Wait
    | Act TreeLeafAct


type DecisionPathNode
    = DescribeBranch String DecisionPathNode
    | EndDecisionPath EndDecisionPathStructure


type alias BotState =
    { programState :
        Maybe
            { decision : DecisionPathNode
            , lastStepIndexInSequence : Int
            }
    , botMemory : BotMemory
    }


type alias BotMemory =
    { lastDockedStationNameFromInfoPanel : Maybe String
    }


type alias State =
    EveOnline.BotFramework.StateIncludingFramework BotState


generalStepDelayMilliseconds : Int
generalStepDelayMilliseconds =
    2000


{-| A first outline of the decision tree for a mining bot is coming from <https://forum.botengine.org/t/how-to-automate-mining-asteroids-in-eve-online/628/109?u=viir>
-}
decideNextAction : BotMemory -> ParsedUserInterface -> DecisionPathNode
decideNextAction botMemory parsedUserInterface =
    if parsedUserInterface |> isShipWarpingOrJumping then
        -- TODO: Look also on the previous memory reading.
        DescribeBranch "I see we are warping." (EndDecisionPath Wait)

    else
        -- TODO: For robustness, also look also on the previous memory reading. Only continue when both indicate is undocked.
        case parsedUserInterface.shipUI of
            CanNotSeeIt ->
                DescribeBranch "I see no ship UI, assume we are docked." (decideNextActionWhenDocked parsedUserInterface)

            CanSee shipUI ->
                case parsedUserInterface.overviewWindow of
                    CanNotSeeIt ->
                        DescribeBranch "I see no overview window, wait until undocking completed." (EndDecisionPath Wait)

                    CanSee overviewWindow ->
                        if shipUI.hitpointsPercent.shield < runAwayShieldHitpointsThresholdPercent then
                            DescribeBranch "Shield hitpoints are too low, run away." (runAway parsedUserInterface)

                        else
                            DescribeBranch "I see we are in space." (decideNextActionWhenInSpace botMemory shipUI overviewWindow parsedUserInterface)


decideNextActionWhenDocked : ParsedUserInterface -> DecisionPathNode
decideNextActionWhenDocked parsedUserInterface =
    case parsedUserInterface |> inventoryWindowItemHangar of
        Nothing ->
            DescribeBranch "I do not see the item hangar in the inventory." (EndDecisionPath Wait)

        Just itemHangar ->
            case parsedUserInterface |> inventoryWindowSelectedContainerFirstItem of
                Nothing ->
                    DescribeBranch "I see no item in the ore hold. Time to undock."
                        (case parsedUserInterface |> activeShipUiElementFromInventoryWindow of
                            Nothing ->
                                EndDecisionPath Wait

                            Just activeShipEntry ->
                                EndDecisionPath
                                    (Act
                                        { firstAction =
                                            activeShipEntry
                                                |> clickLocationOnInventoryShipEntry
                                                |> effectMouseClickAtLocation MouseButtonRight
                                        , followingSteps =
                                            [ ( "Click menu entry 'undock'."
                                              , lastContextMenuOrSubmenu
                                                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Undock")
                                                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                              )
                                            ]
                                        }
                                    )
                        )

                Just itemInInventory ->
                    DescribeBranch "I see at least one item in the ore hold. Move this to the item hangar."
                        (EndDecisionPath
                            (Act
                                { firstAction =
                                    VolatileHostInterface.SimpleDragAndDrop
                                        { startLocation = itemInInventory.totalDisplayRegion |> centerFromDisplayRegion
                                        , endLocation = itemHangar.totalDisplayRegion |> centerFromDisplayRegion
                                        , mouseButton = MouseButtonLeft
                                        }
                                , followingSteps = []
                                }
                            )
                        )


decideNextActionWhenInSpace : BotMemory -> ShipUI -> OverviewWindow -> ParsedUserInterface -> DecisionPathNode
decideNextActionWhenInSpace botMemory shipUI overviewWindow parsedUserInterface =
    let
        mineAsteroidsDecision =
            mineAsteroids botMemory
    in
    if shipUI.hitpointsPercent.shield <= enterCombatShieldHitpointsThresholdPercent then
        DescribeBranch "Shield hitpoints are low enough to enter defense."
            (combat botMemory overviewWindow parsedUserInterface mineAsteroidsDecision)

    else
        mineAsteroidsDecision parsedUserInterface


mineAsteroids : BotMemory -> ParsedUserInterface -> DecisionPathNode
mineAsteroids botMemory parsedUserInterface =
    case parsedUserInterface |> oreHoldFillPercent of
        Nothing ->
            DescribeBranch "I cannot see the ore hold capacity gauge." (EndDecisionPath Wait)

        Just fillPercent ->
            if 99 <= fillPercent then
                DescribeBranch "The ore hold is full enough. Dock to station."
                    (case botMemory.lastDockedStationNameFromInfoPanel of
                        Nothing ->
                            DescribeBranch "At which station should I dock?. I was never docked in a station in this session." (EndDecisionPath Wait)

                        Just lastDockedStationNameFromInfoPanel ->
                            dockToStation { stationNameFromInfoPanel = lastDockedStationNameFromInfoPanel } parsedUserInterface
                    )

            else
                DescribeBranch "The ore hold is not full enough yet. Get more ore."
                    (case parsedUserInterface.targets |> List.head of
                        Nothing ->
                            DescribeBranch "I see no locked target." (acquireLockedTargetForMining parsedUserInterface)

                        Just _ ->
                            DescribeBranch "I see a locked target."
                                (case parsedUserInterface |> shipUiMiningModules |> List.filter (.isActive >> Maybe.withDefault False >> not) |> List.head of
                                    -- TODO: Check previous memory reading too for module activity.
                                    Nothing ->
                                        DescribeBranch "All mining laser modules are active." (EndDecisionPath Wait)

                                    Just inactiveModule ->
                                        DescribeBranch "I see an inactive mining module. Click on it to activate."
                                            (EndDecisionPath
                                                (Act
                                                    { firstAction = inactiveModule.uiNode |> clickOnUIElement MouseButtonLeft
                                                    , followingSteps = []
                                                    }
                                                )
                                            )
                                )
                    )


acquireLockedTargetForMining : ParsedUserInterface -> DecisionPathNode
acquireLockedTargetForMining parsedUserInterface =
    case parsedUserInterface |> topmostAsteroidFromOverviewWindow of
        Nothing ->
            DescribeBranch "I see no asteroid in the overview. Warp to mining site."
                (warpToMiningSite parsedUserInterface)

        Just asteroidInOverview ->
            if asteroidInOverview |> overviewWindowEntryIsInMiningRange |> Maybe.withDefault False then
                DescribeBranch "Asteroid is in range. Lock target."
                    (EndDecisionPath
                        (Act
                            { firstAction = asteroidInOverview.uiNode |> clickOnUIElement MouseButtonRight
                            , followingSteps =
                                [ ( "Click menu entry 'lock'."
                                  , lastContextMenuOrSubmenu
                                        >> Maybe.andThen (menuEntryContainingTextIgnoringCase "lock")
                                        >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                  )
                                ]
                            }
                        )
                    )

            else
                DescribeBranch "Asteroid is not in range. Approach."
                    (if parsedUserInterface |> isShipApproaching then
                        DescribeBranch "record the approach click, and send the bot in a wait loop" (EndDecisionPath Wait)

                     else
                        EndDecisionPath
                            (Act
                                { firstAction = asteroidInOverview.uiNode |> clickOnUIElement MouseButtonRight
                                , followingSteps =
                                    [ ( "Click menu entry 'approach'."
                                      , lastContextMenuOrSubmenu
                                            >> Maybe.andThen (menuEntryContainingTextIgnoringCase "approach")
                                            >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                      )
                                    ]
                                }
                            )
                    )


acquireLockedTargetForCombat : OverviewWindowEntry -> DecisionPathNode
acquireLockedTargetForCombat overviewEntry =
    if overviewEntry |> overviewWindowEntryIsInCombatRange |> Maybe.withDefault False then
        DescribeBranch "Overview entry is in range. Lock target."
            (EndDecisionPath
                (Act
                    { firstAction = overviewEntry.uiNode |> clickOnUIElement MouseButtonRight
                    , followingSteps =
                        [ ( "Click menu entry 'lock'."
                          , lastContextMenuOrSubmenu
                                >> Maybe.andThen (menuEntryMatchingTextIgnoringCase "lock target")
                                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                          )
                        ]
                    }
                )
            )

    else
        DescribeBranch "Overview entry is not in range. Approach."
            (EndDecisionPath
                (Act
                    { firstAction = overviewEntry.uiNode |> clickOnUIElement MouseButtonRight
                    , followingSteps =
                        [ ( "Click menu entry 'approach'."
                          , lastContextMenuOrSubmenu
                                >> Maybe.andThen (menuEntryContainingTextIgnoringCase "approach")
                                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                          )
                        ]
                    }
                )
            )


dockToStation : { stationNameFromInfoPanel : String } -> ParsedUserInterface -> DecisionPathNode
dockToStation { stationNameFromInfoPanel } =
    useContextMenuOnListSurroundingsButton
        [ ( "Click on menu entry 'stations'."
          , lastContextMenuOrSubmenu
                >> Maybe.andThen (menuEntryContainingTextIgnoringCase "stations")
                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
          )
        , ( "Click on menu entry representing the station '" ++ stationNameFromInfoPanel ++ "'."
          , lastContextMenuOrSubmenu
                >> Maybe.andThen
                    (.entries
                        >> List.filter
                            (menuEntryMatchesStationNameFromLocationInfoPanel stationNameFromInfoPanel)
                        >> List.head
                    )
                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
          )
        , ( "Click on menu entry 'dock'"
          , lastContextMenuOrSubmenu
                >> Maybe.andThen (menuEntryContainingTextIgnoringCase "dock")
                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
          )
        ]


warpToMiningSite : ParsedUserInterface -> DecisionPathNode
warpToMiningSite parsedUserInterface =
    parsedUserInterface
        |> useContextMenuOnListSurroundingsButton
            [ ( "Click on menu entry 'asteroid belts'."
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "asteroid belts")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click on one of the menu entries."
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen
                        (.entries >> listElementAtWrappedIndex (getEntropyIntFromUserInterface parsedUserInterface))
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click menu entry 'Warp to Within'"
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Warp to Within")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click menu entry 'Within 0 m'"
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Within 0 m")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            ]


runAway : ParsedUserInterface -> DecisionPathNode
runAway parsedUserInterface =
    parsedUserInterface
        |> useContextMenuOnListSurroundingsButton
            [ ( "Click on menu entry 'planets'."
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "planets")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click on one of the menu entries."
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen
                        (.entries >> listElementAtWrappedIndex (getEntropyIntFromUserInterface parsedUserInterface))
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click menu entry 'Warp to Within'"
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Warp to Within")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            , ( "Click menu entry 'Within 0 m'"
              , lastContextMenuOrSubmenu
                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Within 0 m")
                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
              )
            ]


combat : BotMemory -> OverviewWindow -> ParsedUserInterface -> (ParsedUserInterface -> DecisionPathNode) -> DecisionPathNode
combat botMemory overviewWindow parsedUserInterface continueIfCombatComplete =
    let
        overviewEntriesToAttack =
            overviewWindow.entries
                |> List.sortBy (.distanceInMeters >> Result.withDefault 999999)
                |> List.filter shouldAttackOverviewEntry

        overviewEntriesToLock =
            overviewEntriesToAttack
                |> List.filter (overviewEntryIsAlreadyTargetedOrTargeting >> not)

        targetsToUnlock =
            if overviewEntriesToAttack |> List.any overviewEntryIsActiveTarget then
                []

            else
                parsedUserInterface.targets |> List.filter .isActiveTarget
    in
    case targetsToUnlock |> List.head of
        Just targetToUnlock ->
            DescribeBranch "I see a target to unlock."
                (EndDecisionPath
                    (Act
                        { firstAction =
                            targetToUnlock.barAndImageCont
                                |> Maybe.withDefault targetToUnlock.uiNode
                                |> clickOnUIElement MouseButtonRight
                        , followingSteps =
                            [ ( "Click menu entry 'unlock'."
                              , lastContextMenuOrSubmenu
                                    >> Maybe.andThen (menuEntryContainingTextIgnoringCase "unlock")
                                    >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                              )
                            ]
                        }
                    )
                )

        Nothing ->
            case parsedUserInterface.targets |> List.head of
                Nothing ->
                    DescribeBranch "I see no locked target."
                        (case overviewEntriesToLock of
                            [] ->
                                DescribeBranch "I see no overview entry to lock."
                                    (returnDronesToBay parsedUserInterface
                                        |> Maybe.withDefault
                                            (DescribeBranch "No drones to return." (continueIfCombatComplete parsedUserInterface))
                                    )

                            nextOverviewEntryToLock :: _ ->
                                DescribeBranch "I see an overview entry to lock."
                                    (acquireLockedTargetForCombat nextOverviewEntryToLock)
                        )

                Just _ ->
                    DescribeBranch "I see a locked target."
                        (launchAndEngageDrones parsedUserInterface
                            |> Maybe.withDefault (DescribeBranch "No idling drones." (EndDecisionPath Wait))
                        )


shouldAttackOverviewEntry : EveOnline.MemoryReading.OverviewWindowEntry -> Bool
shouldAttackOverviewEntry =
    iconSpriteHasColorOfRat


iconSpriteHasColorOfRat : EveOnline.MemoryReading.OverviewWindowEntry -> Bool
iconSpriteHasColorOfRat =
    .iconSpriteColorPercent
        >> Maybe.map
            (\colorPercent ->
                colorPercent.g * 3 < colorPercent.r && colorPercent.b * 3 < colorPercent.r && 60 < colorPercent.r && 50 < colorPercent.a
            )
        >> Maybe.withDefault False


launchAndEngageDrones : ParsedUserInterface -> Maybe DecisionPathNode
launchAndEngageDrones parsedUserInterface =
    parsedUserInterface.dronesWindow
        |> maybeNothingFromCanNotSeeIt
        |> Maybe.andThen
            (\dronesWindow ->
                case ( dronesWindow.droneGroupInBay, dronesWindow.droneGroupInLocalSpace ) of
                    ( Just droneGroupInBay, Just droneGroupInLocalSpace ) ->
                        let
                            idlingDrones =
                                droneGroupInLocalSpace.drones
                                    |> List.filter (.uiNode >> .uiNode >> EveOnline.MemoryReading.getAllContainedDisplayTexts >> List.any (String.toLower >> String.contains "idle"))

                            dronesInBayQuantity =
                                droneGroupInBay.header.quantityFromTitle |> Maybe.withDefault 0

                            dronesInLocalSpaceQuantity =
                                droneGroupInLocalSpace.header.quantityFromTitle |> Maybe.withDefault 0
                        in
                        if 0 < (idlingDrones |> List.length) then
                            Just
                                (DescribeBranch "Engage idling drone(s)"
                                    (EndDecisionPath
                                        (Act
                                            { firstAction = droneGroupInLocalSpace.header.uiNode |> clickOnUIElement MouseButtonRight
                                            , followingSteps =
                                                [ ( "Click menu entry 'engage target'."
                                                  , lastContextMenuOrSubmenu
                                                        >> Maybe.andThen (menuEntryContainingTextIgnoringCase "engage target")
                                                        >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                                  )
                                                ]
                                            }
                                        )
                                    )
                                )

                        else if 0 < dronesInBayQuantity && dronesInLocalSpaceQuantity < 5 then
                            Just
                                (DescribeBranch "Launch drones"
                                    (EndDecisionPath
                                        (Act
                                            { firstAction = droneGroupInBay.header.uiNode |> clickOnUIElement MouseButtonRight
                                            , followingSteps =
                                                [ ( "Click menu entry 'Launch drone'."
                                                  , lastContextMenuOrSubmenu
                                                        >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Launch drone")
                                                        >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                                  )
                                                ]
                                            }
                                        )
                                    )
                                )

                        else
                            Nothing

                    _ ->
                        Nothing
            )


returnDronesToBay : ParsedUserInterface -> Maybe DecisionPathNode
returnDronesToBay parsedUserInterface =
    parsedUserInterface.dronesWindow
        |> maybeNothingFromCanNotSeeIt
        |> Maybe.andThen .droneGroupInLocalSpace
        |> Maybe.andThen
            (\droneGroupInLocalSpace ->
                if 0 < (droneGroupInLocalSpace.header.quantityFromTitle |> Maybe.withDefault 0) then
                    Just
                        (DescribeBranch "I see there are drones in local space. Return those to bay."
                            (EndDecisionPath
                                (Act
                                    { firstAction = droneGroupInLocalSpace.header.uiNode |> clickOnUIElement MouseButtonRight
                                    , followingSteps =
                                        [ ( "Click menu entry 'Return to drone bay'."
                                          , lastContextMenuOrSubmenu
                                                >> Maybe.andThen (menuEntryContainingTextIgnoringCase "Return to drone bay")
                                                >> Maybe.map (.uiNode >> clickOnUIElement MouseButtonLeft)
                                          )
                                        ]
                                    }
                                )
                            )
                        )

                else
                    Nothing
            )


useContextMenuOnListSurroundingsButton : List ( String, ParsedUserInterface -> Maybe VolatileHostInterface.EffectOnWindowStructure ) -> ParsedUserInterface -> DecisionPathNode
useContextMenuOnListSurroundingsButton followingSteps parsedUserInterface =
    case parsedUserInterface.infoPanelLocationInfo of
        CanNotSeeIt ->
            DescribeBranch "I cannot see the location info panel." (EndDecisionPath Wait)

        CanSee infoPanelLocationInfo ->
            EndDecisionPath
                (Act
                    { firstAction = infoPanelLocationInfo.listSurroundingsButton |> clickOnUIElement MouseButtonLeft
                    , followingSteps = followingSteps
                    }
                )


initState : State
initState =
    EveOnline.BotFramework.initState
        { programState = Nothing
        , botMemory = { lastDockedStationNameFromInfoPanel = Nothing }
        }


processEvent : InterfaceToHost.BotEvent -> State -> ( State, InterfaceToHost.BotResponse )
processEvent =
    EveOnline.BotFramework.processEvent processEveOnlineBotEvent


processEveOnlineBotEvent :
    EveOnline.BotFramework.BotEventContext
    -> EveOnline.BotFramework.BotEvent
    -> BotState
    -> ( BotState, EveOnline.BotFramework.BotEventResponse )
processEveOnlineBotEvent eventContext event stateBefore =
    case event of
        EveOnline.BotFramework.MemoryReadingCompleted parsedUserInterface ->
            let
                botMemory =
                    stateBefore.botMemory |> integrateCurrentReadingsIntoBotMemory parsedUserInterface

                programStateBefore =
                    stateBefore.programState
                        |> Maybe.withDefault { decision = decideNextAction botMemory parsedUserInterface, lastStepIndexInSequence = 0 }

                ( decisionStagesDescriptions, decisionLeaf ) =
                    unpackToDecisionStagesDescriptionsAndLeaf programStateBefore.decision

                ( currentStepDescription, effectsOnGameClientWindow, programState ) =
                    case decisionLeaf of
                        Wait ->
                            ( "Wait", [], Nothing )

                        Act act ->
                            let
                                programStateAdvancedToNextStep =
                                    { programStateBefore
                                        | lastStepIndexInSequence = programStateBefore.lastStepIndexInSequence + 1
                                    }

                                stepsIncludingFirstAction =
                                    ( "", always (Just act.firstAction) ) :: act.followingSteps
                            in
                            case stepsIncludingFirstAction |> List.drop programStateBefore.lastStepIndexInSequence |> List.head of
                                Nothing ->
                                    ( "Completed sequence.", [], Nothing )

                                Just ( stepDescription, effectOnGameClientWindowFromUserInterface ) ->
                                    case parsedUserInterface |> effectOnGameClientWindowFromUserInterface of
                                        Nothing ->
                                            ( "Failed step: " ++ stepDescription, [], Nothing )

                                        Just effect ->
                                            ( stepDescription, [ effect ], Just programStateAdvancedToNextStep )

                effectsRequests =
                    effectsOnGameClientWindow |> List.map EveOnline.BotFramework.EffectOnGameClientWindow

                describeActivity =
                    (decisionStagesDescriptions ++ [ currentStepDescription ])
                        |> List.indexedMap
                            (\decisionLevel -> (++) (("+" |> List.repeat (decisionLevel + 1) |> String.join "") ++ " "))
                        |> String.join "\n"

                statusMessage =
                    [ parsedUserInterface |> describeUserInterfaceForMonitoring, describeActivity ]
                        |> String.join "\n"
            in
            ( { stateBefore | botMemory = botMemory, programState = programState }
            , EveOnline.BotFramework.ContinueSession
                { effects = effectsRequests
                , millisecondsToNextReadingFromGame = generalStepDelayMilliseconds
                , statusDescriptionText = statusMessage
                }
            )


describeUserInterfaceForMonitoring : ParsedUserInterface -> String
describeUserInterfaceForMonitoring parsedUserInterface =
    let
        describeShip =
            case parsedUserInterface.shipUI of
                CanSee shipUI ->
                    "I am in space, shield HP at " ++ (shipUI.hitpointsPercent.shield |> String.fromInt) ++ "%."

                CanNotSeeIt ->
                    case parsedUserInterface.infoPanelLocationInfo |> maybeVisibleAndThen .expandedContent |> maybeNothingFromCanNotSeeIt |> Maybe.andThen .currentStationName of
                        Just stationName ->
                            "I am docked at '" ++ stationName ++ "'."

                        Nothing ->
                            "I cannot see if I am docked or in space. Please set up game client first."

        describeOreHold =
            "Ore hold filled " ++ (parsedUserInterface |> oreHoldFillPercent |> Maybe.map String.fromInt |> Maybe.withDefault "Unknown") ++ "%."
    in
    [ describeShip, describeOreHold ] |> String.join " "


integrateCurrentReadingsIntoBotMemory : ParsedUserInterface -> BotMemory -> BotMemory
integrateCurrentReadingsIntoBotMemory currentReading botMemoryBefore =
    let
        currentStationNameFromInfoPanel =
            currentReading.infoPanelLocationInfo
                |> maybeVisibleAndThen .expandedContent
                |> maybeNothingFromCanNotSeeIt
                |> Maybe.andThen .currentStationName
    in
    { lastDockedStationNameFromInfoPanel =
        [ currentStationNameFromInfoPanel, botMemoryBefore.lastDockedStationNameFromInfoPanel ]
            |> List.filterMap identity
            |> List.head
    }


unpackToDecisionStagesDescriptionsAndLeaf : DecisionPathNode -> ( List String, EndDecisionPathStructure )
unpackToDecisionStagesDescriptionsAndLeaf node =
    case node of
        EndDecisionPath leaf ->
            ( [], leaf )

        DescribeBranch branchDescription childNode ->
            let
                ( childDecisionsDescriptions, leaf ) =
                    unpackToDecisionStagesDescriptionsAndLeaf childNode
            in
            ( branchDescription :: childDecisionsDescriptions, leaf )


activeShipUiElementFromInventoryWindow : ParsedUserInterface -> Maybe UIElement
activeShipUiElementFromInventoryWindow =
    .inventoryWindows
        >> List.head
        >> Maybe.map .leftTreeEntries
        -- Assume upmost entry is active ship.
        >> Maybe.andThen (List.sortBy (.uiNode >> .totalDisplayRegion >> .y) >> List.head)
        >> Maybe.map .uiNode


{-| Assume upper row of modules only contains mining modules.
-}
shipUiMiningModules : ParsedUserInterface -> List ShipUIModule
shipUiMiningModules =
    shipUiModulesRows >> List.head >> Maybe.withDefault []


shipUiModules : ParsedUserInterface -> List ShipUIModule
shipUiModules =
    .shipUI >> maybeNothingFromCanNotSeeIt >> Maybe.map .modules >> Maybe.withDefault []


{-| Groups the modules into rows.
-}
shipUiModulesRows : ParsedUserInterface -> List (List ShipUIModule)
shipUiModulesRows =
    let
        putModulesInSameGroup moduleA moduleB =
            let
                distanceY =
                    (moduleA.uiNode.totalDisplayRegion |> centerFromDisplayRegion).y
                        - (moduleB.uiNode.totalDisplayRegion |> centerFromDisplayRegion).y
            in
            abs distanceY < 10
    in
    shipUiModules
        >> List.sortBy (.uiNode >> .totalDisplayRegion >> .y)
        >> List.foldl
            (\shipModule groups ->
                case groups |> List.filter (List.any (putModulesInSameGroup shipModule)) |> List.head of
                    Nothing ->
                        groups ++ [ [ shipModule ] ]

                    Just matchingGroup ->
                        (groups |> listRemove matchingGroup) ++ [ matchingGroup ++ [ shipModule ] ]
            )
            []


{-| Returns the menu entry containing the string from the parameter `textToSearch`.
If there are multiple such entries, these are sorted by the length of their text, minus whitespaces in the beginning and the end.
The one with the shortest text is returned.
-}
menuEntryContainingTextIgnoringCase : String -> EveOnline.MemoryReading.ContextMenu -> Maybe EveOnline.MemoryReading.ContextMenuEntry
menuEntryContainingTextIgnoringCase textToSearch =
    .entries
        >> List.filter (.text >> String.toLower >> String.contains (textToSearch |> String.toLower))
        >> List.sortBy (.text >> String.trim >> String.length)
        >> List.head


menuEntryMatchingTextIgnoringCase : String -> EveOnline.MemoryReading.ContextMenu -> Maybe EveOnline.MemoryReading.ContextMenuEntry
menuEntryMatchingTextIgnoringCase textToSearch =
    .entries
        >> List.filter (.text >> String.toLower >> (==) (textToSearch |> String.toLower))
        >> List.head


{-| The names are at least sometimes displayed different: 'Moon 7' can become 'M7'
-}
menuEntryMatchesStationNameFromLocationInfoPanel : String -> EveOnline.MemoryReading.ContextMenuEntry -> Bool
menuEntryMatchesStationNameFromLocationInfoPanel stationNameFromInfoPanel menuEntry =
    (stationNameFromInfoPanel |> String.toLower |> String.replace "moon " "m")
        == (menuEntry.text |> String.trim |> String.toLower)


lastContextMenuOrSubmenu : ParsedUserInterface -> Maybe EveOnline.MemoryReading.ContextMenu
lastContextMenuOrSubmenu =
    .contextMenus >> List.head


topmostAsteroidFromOverviewWindow : ParsedUserInterface -> Maybe OverviewWindowEntry
topmostAsteroidFromOverviewWindow =
    overviewWindowEntriesRepresentingAsteroids
        >> List.sortBy (.uiNode >> .totalDisplayRegion >> .y)
        >> List.head


overviewWindowEntryIsInMiningRange : OverviewWindowEntry -> Maybe Bool
overviewWindowEntryIsInMiningRange =
    .distanceInMeters >> Result.map (\distanceInMeters -> distanceInMeters < 1000) >> Result.toMaybe


overviewWindowEntryIsInCombatRange : OverviewWindowEntry -> Maybe Bool
overviewWindowEntryIsInCombatRange =
    .distanceInMeters >> Result.map (\distanceInMeters -> distanceInMeters < 5000) >> Result.toMaybe


overviewWindowEntriesRepresentingAsteroids : ParsedUserInterface -> List OverviewWindowEntry
overviewWindowEntriesRepresentingAsteroids =
    .overviewWindow
        >> maybeNothingFromCanNotSeeIt
        >> Maybe.map (.entries >> List.filter overviewWindowEntryRepresentsAnAsteroid)
        >> Maybe.withDefault []


overviewWindowEntryRepresentsAnAsteroid : OverviewWindowEntry -> Bool
overviewWindowEntryRepresentsAnAsteroid entry =
    (entry.textsLeftToRight |> List.any (String.toLower >> String.contains "asteroid"))
        && (entry.textsLeftToRight |> List.any (String.toLower >> String.contains "belt") |> not)


oreHoldFillPercent : ParsedUserInterface -> Maybe Int
oreHoldFillPercent =
    .inventoryWindows
        >> List.head
        >> Maybe.andThen .selectedContainerCapacityGauge
        >> Maybe.map (\capacity -> capacity.used * 100 // capacity.maximum)


inventoryWindowSelectedContainerFirstItem : ParsedUserInterface -> Maybe UIElement
inventoryWindowSelectedContainerFirstItem =
    .inventoryWindows
        >> List.head
        >> Maybe.andThen .selectedContainerInventory
        >> Maybe.andThen (.listViewItems >> List.head)


inventoryWindowItemHangar : ParsedUserInterface -> Maybe UIElement
inventoryWindowItemHangar =
    .inventoryWindows
        >> List.head
        >> Maybe.map .leftTreeEntries
        >> Maybe.andThen (List.filter (.text >> String.toLower >> String.contains "item hangar") >> List.head)
        >> Maybe.map .uiNode


overviewEntryIsAlreadyTargetedOrTargeting : EveOnline.MemoryReading.OverviewWindowEntry -> Bool
overviewEntryIsAlreadyTargetedOrTargeting =
    .namesUnderSpaceObjectIcon
        >> Set.intersect ([ "targetedByMeIndicator", "targeting" ] |> Set.fromList)
        >> Set.isEmpty
        >> not


overviewEntryIsActiveTarget : EveOnline.MemoryReading.OverviewWindowEntry -> Bool
overviewEntryIsActiveTarget =
    .namesUnderSpaceObjectIcon
        >> Set.member "myActiveTargetIndicator"


clickOnUIElement : MouseButton -> UIElement -> VolatileHostInterface.EffectOnWindowStructure
clickOnUIElement mouseButton uiElement =
    effectMouseClickAtLocation mouseButton (uiElement.totalDisplayRegion |> centerFromDisplayRegion)


{-| The region of a ship entry in the inventory window can contain child nodes (e.g. 'Ore Hold').
For this reason, we don't click on the center but stay close to the top.
-}
clickLocationOnInventoryShipEntry : UIElement -> VolatileHostInterface.Location2d
clickLocationOnInventoryShipEntry uiElement =
    { x = uiElement.totalDisplayRegion.x + uiElement.totalDisplayRegion.width // 2
    , y = uiElement.totalDisplayRegion.y + 7
    }


isShipWarpingOrJumping : ParsedUserInterface -> Bool
isShipWarpingOrJumping =
    .shipUI
        >> maybeNothingFromCanNotSeeIt
        >> Maybe.andThen (.indication >> maybeNothingFromCanNotSeeIt)
        >> Maybe.andThen (.maneuverType >> maybeNothingFromCanNotSeeIt)
        >> Maybe.map
            (\maneuverType ->
                [ EveOnline.MemoryReading.ManeuverWarp, EveOnline.MemoryReading.ManeuverJump ]
                    |> List.member maneuverType
            )
        -- If the ship is just floating in space, there might be no indication displayed.
        >> Maybe.withDefault False


isShipApproaching : ParsedUserInterface -> Bool
isShipApproaching =
    .shipUI
        >> maybeNothingFromCanNotSeeIt
        >> Maybe.andThen (.indication >> maybeNothingFromCanNotSeeIt)
        >> Maybe.andThen (.maneuverType >> maybeNothingFromCanNotSeeIt)
        >> Maybe.map ((==) EveOnline.MemoryReading.ManeuverApproach)
        -- If the ship is just floating in space, there might be no indication displayed.
        >> Maybe.withDefault False


listElementAtWrappedIndex : Int -> List element -> Maybe element
listElementAtWrappedIndex indexToWrap list =
    if (list |> List.length) < 1 then
        Nothing

    else
        list |> List.drop (indexToWrap |> modBy (list |> List.length)) |> List.head


listRemove : element -> List element -> List element
listRemove elementToRemove =
    List.filter ((/=) elementToRemove)
