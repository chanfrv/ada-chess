with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;


procedure llr_5_4_1_check_pass is
    testboard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- a
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- b
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- c
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- d
        (WKing, WQueen, Empty, Empty, BRook, Empty, Empty, BKing), -- e
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- f
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty), -- g
        (Empty, Empty,  Empty, Empty, Empty, Empty, Empty, Empty)  -- h
    );
    
    res : Boolean;
begin
    res := IsValidMove(testboard, (e,2), (b,5), False, (IsEmpty => True));
    if res = True then
        OS_Exit(1);
    end if;
end llr_5_4_1_check_pass;
