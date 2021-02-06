with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;


procedure llr_5_2_1_valid_pass is
    testboard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (WRook,   WPawn, Empty, Empty, Empty, Empty, BPawn, BRook  ), -- a
        (WKnight, WPawn, Empty, Empty, Empty, Empty, BPawn, BKnight), -- b
        (WBishop, WPawn, Empty, Empty, Empty, Empty, BPawn, BBishop), -- c
        (WQueen,  WPawn, Empty, Empty, Empty, Empty, BPawn, BQueen ), -- d
        (WKing,   WPawn, Empty, Empty, Empty, Empty, BPawn, BKing  ), -- e
        (WBishop, WPawn, Empty, Empty, Empty, Empty, BPawn, BBishop), -- f
        (WKnight, WPawn, Empty, Empty, Empty, Empty, BPawn, BKnight), -- g
        (WRook,   WPawn, Empty, Empty, Empty, Empty, BPawn, BRook  )  -- h
    );
    
    res : Boolean;
begin
    res := IsValidMove_Pawn(testboard, (e,2), (e,4), (IsEmpty => True));
    if res = False then
        OS_Exit(1);
    end if;
    
    res := IsValidMove_Bishop(testboard, (e,2), (e,4));
    if res = True then
        OS_Exit(2);
    end if;
    
    res := IsValidMove_King(testboard, (e,2), (e,4));
    if res = True then
        OS_Exit(3);
    end if;
    
end llr_5_2_1_valid_pass;
