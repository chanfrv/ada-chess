with Ada.Text_IO; use Ada.Text_IO;
with Board.Parse; use Board.Parse;


package body Board.EnPassant is

   
    function EnPassant_Handler(Board   : in out Board_t;
                               Piece   : in Piece_t;
                               Color   : in Color_t;
                               Capture : in Boolean;
                               From    : in Coordinates_t;
                               To      : in Coordinates_t) return EnPassant_Coordinates_t
    is
    begin        
        -- Execute en passant from the last move
        if Piece = Pawn and Capture = True
          and (EnPassant_Coords.IsEnPassant and then To = EnPassant_Coords.To)
        then
            Put_Line("En passant on " & Image(EnPassant_Coords.Target) & " pawn");
            Board(EnPassant_Coords.Target.File, EnPassant_Coords.Target.Rank) := (IsEmpty => True);
        end if;

        EnPassant_Coords := (IsEnPassant => False);
        -- Register the pawn for en passant
        if Piece = Pawn and abs (From.Rank - To.Rank) = 2 then
            case Color is
                when White =>
                    EnPassant_Coords := (True, (From.File, 3), To);
                when Black =>
                    EnPassant_Coords := (True, (From.File, 6), To);
            end case;
        end if;
        
        return EnPassant_Coords;
    end EnPassant_Handler;
    

end Board.EnPassant;
