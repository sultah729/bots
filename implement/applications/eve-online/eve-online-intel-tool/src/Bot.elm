{- EVE Online Intel Bot - Local Watch Script
   This bot watches local and plays an alarm sound when a pilot with bad standing appears.

   bot-catalog-tags:eve-online,intel,local-watch
   authors-forum-usernames:viir
-}


module Bot exposing
    ( State
    , initState
    , processEvent
    )

import BotEngine.Interface_To_Host_20190808 as InterfaceToHost
import Sanderling.MemoryReading
    exposing
        ( MaybeVisible(..)
        , ParsedUserInterface
        , canNotSeeItFromMaybeNothing
        )
import Sanderling.SimpleSanderling as SimpleSanderling exposing (BotEventAtTime, BotRequest(..))


{-| The bot does not need to remember anything from the past; the information on the game client screen is sufficient to decide what to do next.
Therefore we need no state and use an empty tuple '()' to define the type of the state.
-}
type alias BotState =
    ()


type alias State =
    SimpleSanderling.StateIncludingSetup BotState


goodStandingPatterns : List String
goodStandingPatterns =
    [ "good standing", "excellent standing", "is in your" ]


initState : State
initState =
    SimpleSanderling.initState ()


processEvent : InterfaceToHost.BotEvent -> State -> ( State, InterfaceToHost.BotResponse )
processEvent =
    SimpleSanderling.processEvent processEveOnlineBotEvent


processEveOnlineBotEvent :
    BotEventAtTime
    -> BotState
    -> { newState : BotState, requests : List BotRequest, millisecondsToNextMemoryReading : Int, statusDescriptionText : String }
processEveOnlineBotEvent eventAtTime stateBefore =
    case eventAtTime.event of
        SimpleSanderling.MemoryReadingCompleted memoryReading ->
            let
                ( requests, statusMessage ) =
                    botRequestsFromGameClientState memoryReading
            in
            { newState = stateBefore
            , requests = requests
            , millisecondsToNextMemoryReading = 2000
            , statusDescriptionText = statusMessage
            }

        SimpleSanderling.SetBotConfiguration botConfiguration ->
            { newState = stateBefore
            , requests = []
            , millisecondsToNextMemoryReading = 2000
            , statusDescriptionText =
                if botConfiguration |> String.isEmpty then
                    ""

                else
                    "I have a problem with this configuration: I am not programmed to support configuration at all. Maybe the bot catalog (https://to.botengine.org/bot-catalog) has a bot which better matches your use case?"
            }


botRequestsFromGameClientState : ParsedUserInterface -> ( List BotRequest, String )
botRequestsFromGameClientState parsedUserInterface =
    case parsedUserInterface |> localChatWindowFromUserInterface of
        CanNotSeeIt ->
            ( [ SimpleSanderling.ConsoleBeepSequenceRequest
                    [ { frequency = 700, durationInMs = 100 }
                    , { frequency = 0, durationInMs = 100 }
                    , { frequency = 700, durationInMs = 100 }
                    , { frequency = 400, durationInMs = 100 }
                    ]
              ]
            , "I don't see the local chat window."
            )

        CanSee localChatWindow ->
            let
                chatUserHasGoodStanding chatUser =
                    goodStandingPatterns
                        |> List.any
                            (\goodStandingPattern ->
                                chatUser.standingIconHint
                                    |> Maybe.map (String.toLower >> String.contains goodStandingPattern)
                                    |> Maybe.withDefault False
                            )

                subsetOfUsersWithNoGoodStanding =
                    localChatWindow.visibleUsers
                        |> List.filter (chatUserHasGoodStanding >> not)

                chatWindowReport =
                    "I see "
                        ++ (localChatWindow.visibleUsers |> List.length |> String.fromInt)
                        ++ " users in the local chat. "
                        ++ (subsetOfUsersWithNoGoodStanding |> List.length |> String.fromInt)
                        ++ " with no good standing."

                alarmRequests =
                    if 1 < (subsetOfUsersWithNoGoodStanding |> List.length) then
                        [ SimpleSanderling.ConsoleBeepSequenceRequest
                            [ { frequency = 700, durationInMs = 100 }
                            , { frequency = 0, durationInMs = 100 }
                            , { frequency = 700, durationInMs = 500 }
                            ]
                        ]

                    else
                        []
            in
            ( alarmRequests, chatWindowReport )


localChatWindowFromUserInterface : ParsedUserInterface -> MaybeVisible Sanderling.MemoryReading.ChatWindow
localChatWindowFromUserInterface =
    .chatWindowStacks
        >> List.filterMap .chatWindow
        >> List.filter (.name >> Maybe.map (String.endsWith "_local") >> Maybe.withDefault False)
        >> List.head
        >> canNotSeeItFromMaybeNothing
