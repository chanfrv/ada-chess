with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;
with Board.Strings.Parse; use Board.Strings.Parse;


procedure llr_6_1_1_enpassant_pass is
    testboard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- a
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- b
        (Empty, Empty, Empty, Empty, WPawn, Empty, Empty, Empty), -- c
        (Empty, Empty, Empty, Empty, Empty, Empty, BPawn, Empty), -- d
        (WKing, Empty, Empty, Empty, Empty, Empty, Empty, BKing), -- e
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- f
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- g
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)  -- h
    );
    
    cm  : Move_t;
    res : Boolean;
    mr  : MoveResult_t;
begin
    res := Traverse("d5", cm); -- black move
    mr := Move(testboard, cm, Black);
    
    res := Traverse("xd6", cm); -- white en passant
    mr := Move(testboard, cm, White);
    
    if testboard(c, 5) /= Empty and testboard(c, 5) /= Empty and testboard(d, 6) /= WPawn then
        OS_Exit(1);
    end if;
end llr_6_1_1_enpassant_pass;
