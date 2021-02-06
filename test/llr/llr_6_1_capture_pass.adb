with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;
with Board.Strings.Parse; use Board.Strings.Parse;


procedure llr_6_1_capture_pass is
    testboard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty), -- a
        (Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty), -- b
        (Empty, Empty, Empty, Empty, BBishop, Empty, Empty, Empty), -- c
        (Empty, Empty, Empty, WPawn, Empty,   Empty, Empty, Empty), -- d
        (WKing, Empty, Empty, Empty, Empty,   Empty, Empty, BKing), -- e
        (Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty), -- f
        (Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty), -- g
        (Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty)  -- h
    );
    
    cm  : Move_t;
    res : Boolean;
    mr  : MoveResult_t;
begin
    res := Traverse("xc5", cm);
    mr := Move(testboard, cm, White);
    
    if testboard(d, 4) /= Empty and testboard(c, 5) /= WPawn then
        OS_Exit(1);
    end if;
end llr_6_1_capture_pass;
