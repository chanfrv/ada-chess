package Board.Castling is

    
    -- For a castling move to be valid, neither the king nor the rook must
    -- have moved.
    type Castling_Validity_t is
        record
            White_King_Moved           : Boolean;
            White_Rook_Kingside_Moved  : Boolean;
            White_Rook_Queenside_Moved : Boolean;
            Black_King_Moved           : Boolean;
            Black_Rook_Kingside_Moved  : Boolean;
            Black_Rook_Queenside_Moved : Boolean;
        end record;
    
    
    Castling_Validity : Castling_Validity_t := (White_King_Moved           => False,
                                                White_Rook_Kingside_Moved  => False,
                                                White_Rook_Queenside_Moved => False,
                                                Black_King_Moved           => False,
                                                Black_Rook_Kingside_Moved  => False,
                                                Black_Rook_Queenside_Moved => False);
    
    
    procedure Castling_Unregister(Board : in Board_t; From : in Coordinates_t);
    
    
    procedure Castling_Kingside(Board : in out Board_t; Color : in Color_t);
    
    procedure Castling_Queenside(Board : in out Board_t; Color : in Color_t);
    
    
end Board.Castling;
