with Board.Strings; use Board.Strings;

with Logs;


package body Board.EnPassant is

   
    function EnPassant_Handler(Board   : in out Board_t;
                               Piece   : in Piece_t;
                               Color   : in Color_t;
                               Capture : in Boolean;
                               From    : in Coordinates_t;
                               To      : in Coordinates_t) return EnPassant_Coordinates_t
    is
        Pawn_Rank : Rank_t := (if Color = White then 3 else 6);
    begin        
        -- Execute en passant from the last move
        if Piece = Pawn
          and Capture = True
          and (EnPassant_Coords.IsEnPassant and then To = EnPassant_Coords.To) then
            Logs.Debug("En passant on " & Image(EnPassant_Coords.Target) & " pawn");
            Board(EnPassant_Coords.Target.File, EnPassant_Coords.Target.Rank) := (IsEmpty => True);
        end if;

        -- Remove en passant from last move
        EnPassant_Coords := (IsEnPassant => False);
        
        -- Register the pawn for en passant
        if Piece = Pawn and abs (From.Rank - To.Rank) = 2 then
            EnPassant_Coords := (True, (From.File, Pawn_Rank), To);
        end if;
        
        return EnPassant_Coords;
    end EnPassant_Handler;
    

end Board.EnPassant;
