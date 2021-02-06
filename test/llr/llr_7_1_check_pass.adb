with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;


procedure llr_7_1_check_pass is
    GameState       : GameResult_t;
    testboard       : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- a
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- b
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- c
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- d
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- e
        (Empty, WKing, Empty, Empty, WKnight, WKnight, Empty, Empty), -- f
        (Empty, Empty, Empty, Empty, BPawn, Empty, Empty, Empty), -- g
        (Empty, Empty, Empty, BKing, Empty, Empty, Empty, Empty)  -- h
    );
--    MoveResult      : MoveResult_t;
begin
    GameState := Game_Ended(testboard, White);
    if GameState /= Check then
        OS_Exit(1);
    end if;
end llr_7_1_check_pass;
