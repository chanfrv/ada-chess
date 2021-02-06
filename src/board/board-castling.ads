package Board.Castling is

    
    procedure Castling_Unregister(Board : in Board_t; From : in Coordinates_t);
    
    
    function Castling_Kingside(Board : in out Board_t; Color : in Color_t) return MoveResult_t;
    
    function Castling_Queenside(Board : in out Board_t; Color : in Color_t) return MoveResult_t;
    
    
private
    
    
    -- For a castling move to be valid, neither the king nor the rook must
    -- have moved.
    White_King_Moved           : Boolean := False;
    White_Rook_Kingside_Moved  : Boolean := False;
    White_Rook_Queenside_Moved : Boolean := False;
    Black_King_Moved           : Boolean := False;
    Black_Rook_Kingside_Moved  : Boolean := False;
    Black_Rook_Queenside_Moved : Boolean := False;

    
end Board.Castling;
