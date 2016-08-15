module Utils exposing (twoWayChatChannelFor)

{-
   To determine the 2-way chat channel for these two users, sort them alphabetically, and insert a "<->" between them, prepending with "room:".
   For example:
       > twoWayChatChannelFor "alice" "bob" == twoWayChatChannelFor "bob" "alice"
       > twoWayChatChannelFor "alice" "bob" == "room:alice<->bob"
-}


twoWayChatChannelFor : String -> String -> String
twoWayChatChannelFor user1 user2 =
    case user1 < user2 of
        True ->
            "room:" ++ user1 ++ "<->" ++ user2

        False ->
            "room:" ++ user2 ++ "<->" ++ user1
