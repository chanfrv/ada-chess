with Board; use Board;
with Board.Strings; use Board.Strings;
with Board.Strings.Parse; use Board.Strings.Parse;
with Board.Strings.Pretty; use Board.Strings.Pretty;
with Logs;


procedure Test is

    TestBoard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (WRook,   WPawn,   Empty, Empty, Empty, Empty, BPawn, BRook  ), -- a
        (Empty,   WPawn,   Empty, Empty, Empty, Empty, BPawn, BKnight), -- b
        (WBishop, Empty,   Empty, WPawn, Empty, Empty, BPawn, Empty  ), -- c
        (WQueen,  Empty,   WPawn, Empty, Empty, Empty, Empty, BQueen ), -- d
        (WKing,   Empty,   Empty, BPawn, Empty, Empty, WPawn, BKing  ), -- e
        (Empty,   Empty,   Empty, Empty, Empty, Empty, BPawn, BBishop), -- f
        (WKnight, Empty,   Empty, Empty, Empty, Empty, BPawn, Empty  ), -- g
        (WRook,   WPawn,   Empty, Empty, Empty, Empty, BPawn, BRook  )  -- h
    );

    ret  : GameResult_t;
    mov  : Move_t;
    succ : Boolean;
    res  : MoveResult_t;
begin
    Logs.Set_Level(Logs.Info);

    Pretty_Print(TestBoard);

    ret := Game_Ended(TestBoard, Black);

    Logs.Info(ret'Image);

    succ := Traverse("xd8Q", mov);

    res := Move(TestBoard, mov, White);

    Pretty_Print(TestBoard);

end Test;
