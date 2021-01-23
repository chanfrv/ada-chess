package Board.EnPassant is

    
    -- Optional coordinates, if set then a pawn moving to To will capture
    -- the piece on Target.
    type EnPassant_Coordinates_t(IsEnPassant : Boolean := False) is
        record
            case IsEnPassant is
                when True =>
                    To     : Coordinates_t;
                    Target : Coordinates_t;
                when False =>
                    null;
            end case;
        end record;

    
    -- Global en passant variable
    EnPassant_Coords : EnPassant_Coordinates_t := (IsEnPassant => False);

    
    -- Handler
    function EnPassant_Handler(Board   : in out Board_t;
                               Piece   : in Piece_t;
                               Color   : in Color_t;
                               Capture : in Boolean;
                               From    : in Coordinates_t;
                               To      : in Coordinates_t) return EnPassant_Coordinates_t;
    

end Board.EnPassant;
