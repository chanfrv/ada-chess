with Ada.Text_IO; use Ada.Text_IO;

with Board; use Board;
with Board.Strings; use Board.Strings;


procedure Test is
    Board : Board_t :=
      (--    1        2      3      4      5      6      7      8
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- a
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- b
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- c
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- d
        (WKing, BQueen, Empty, Empty, Empty, Empty, Empty, BKing), -- e
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- f
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty), -- g
        (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)  -- h
      );
    GameState : GameResult_t := Game_Ended(Board, Black);
begin
    --Pretty_Print(Board, White);
    Put_Line("Game state: " & GameState'Image);
end Test;
