with GNAT.Regexp; use GNAT.Regexp;
with Ada.Text_IO; use Ada.Text_IO;

with Board; use Board;
with Board.Strings; use Board.Strings;
with Board.Strings.Parse; use Board.Strings.Parse;
with Board.Strings.Pretty; use Board.Strings.Pretty;
with Logs;


procedure Test is

    --GameState : GameResult_t;
begin
    logs.Set_Level(Logs.Debug);

    --Pretty_Print(Board);

    --GameState := Game_Ended(Board, Black);

    --Logs.Debug("Game state: " & GameState'Image);

end Test;
