with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;
with Board.Strings.Parse; use Board.Strings.Parse;


procedure llr_5_1_2_move_pass is
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
    
    currmove : Move_t;
    res : Boolean;
    move_res : MoveResult_t;
begin
    res := Traverse("e4", currmove);
    
    move_res := Move(testboard, currmove, White);
    if move_res in Invalid_Move | Ambiguous_Move then
        OS_Exit(1);
    end if;
    
    res := Traverse("e4", currmove);
    move_res := Move(testboard, currmove, Black);
    if move_res = Valid_Move then
        OS_Exit(2);
    end if;  
    
    res := Traverse("Nc3", currmove);
    move_res := Move(testboard, currmove, White);
    if move_res in Invalid_Move | Ambiguous_Move then
        OS_Exit(3);
    end if;
    
end llr_5_1_2_move_pass;
