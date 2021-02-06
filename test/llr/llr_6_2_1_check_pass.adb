with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board; use Board;
with Board.Strings.Parse; use Board.Strings.Parse;
with Logs;

procedure llr_6_2_1_check_pass is
   testboard : Board_t :=
    (--    1        2      3      4      5      6      7      8
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- a
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- b
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- c
        (Empty, Empty, Empty, Empty, Empty, Empty, WPawn, Empty), -- d
        (WKing, Empty, Empty, Empty, Empty, Empty, Empty, BKing), -- e
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- f
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- g
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)  -- h
    );
begin
    Logs.Set_Level(Logs.Debug);
    
    if not IsKingCheck(testboard, (e,8)) then
        OS_Exit(1);
    end if;
end llr_6_2_1_check_pass;
