with Logs;


package body Board.Castling is

   
    procedure Castling_Unregister(Board : in Board_t; From : in Coordinates_t)
    is
        Cell : Cell_t := Board(From.File, From.Rank);        
        Side : Castling_t := (if From.File <= d then Queenside else Kingside);
    begin
        -- White king
        if Cell = WKing then               
            Castling_Validity.White_King_Moved := True;
        
        -- White rook
        elsif Cell = WRook then
            -- Queenside rook
            if Side = Queenside then
                Castling_Validity.White_Rook_Queenside_Moved := True;
            -- Kingside rook
            else
                Castling_Validity.White_Rook_Kingside_Moved := True;
            end if;
        
        -- Black king
        elsif Cell = BKing then
            Castling_Validity.Black_King_Moved := True;
        
        -- Black rook
        elsif Cell = BRook then
            -- Queenside rook
            if Side = Queenside then
                Castling_Validity.Black_Rook_Queenside_Moved := True;
            -- Kingside rook
            else
                Castling_Validity.Black_Rook_Kingside_Moved := True;
            end if;
        end if;
    end Castling_Unregister;
    
  
    procedure Castling_Kingside(Board : in out Board_t; Color : in Color_t)
    is
        -- check that the king is never check on its way to the final
        -- castling position
        function Is_King_Checked_Kingside(Board : in Board_t;
                                          Rank  : in Rank_t) return Boolean is
        begin
            return not IsKingCheck(Board, (e, Rank))
              and not IsKingCheckAt(Board, (e, Rank), (f, Rank))
              and not IsKingCheckAt(Board, (e, Rank), (g, Rank));
        end Is_King_Checked_Kingside;
        
        
        Rank : Rank_t := (if Color = White then 1 else 8);
        
        King : Cell_t := (if Color = White then WKing else BKing);
        Rook : Cell_t := (if Color = White then WRook else BRook);
        
        King_Moved : Boolean := (if Color = White then
                                     Castling_Validity.White_King_Moved
                                 else
                                     Castling_Validity.Black_King_Moved);
        Rook_Moved : Boolean := (if Color = White then
                                     Castling_Validity.White_Rook_Kingside_Moved
                                 else
                                     Castling_Validity.Black_Rook_Kingside_Moved);        
    begin
        if (not King_Moved and not Rook_Moved
             and Board(e, Rank) = King
             and Board(f, Rank) = Empty
             and Board(g, Rank) = Empty
             and Board(h, Rank) = Rook)
          and then not Is_King_Checked_Kingside(Board, Rank)
        then
            Board(e, Rank) := Empty;
            Board(f, Rank) := Rook;
            Board(g, Rank) := King;
            Board(h, Rank) := Empty;
        else
            Logs.Debug("Invalid castling");
        end if;
    end Castling_Kingside;
    
    
    procedure Castling_Queenside(Board : in out Board_t; Color : in Color_t)
    is
        -- check that the king is never check on its way to the final
        -- castling position
        function Is_King_Checked_Queenside(Board : in Board_t;
                                           Rank  : in Rank_t) return Boolean is
        begin
            return not IsKingCheck(Board, (e, Rank))
              and not IsKingCheckAt(Board, (e, Rank), (d, Rank))
              and not IsKingCheckAt(Board, (e, Rank), (c, Rank))
              and not IsKingCheckAt(Board, (e, Rank), (b, Rank));
        end Is_King_Checked_Queenside;
        
        
        Rank : Rank_t := (if Color = White then 1 else 8);
        
        King : Cell_t := (if Color = White then WKing else BKing);
        Rook : Cell_t := (if Color = White then WRook else BRook);
        
        King_Moved : Boolean := (if Color = White then
                                     Castling_Validity.White_King_Moved
                                 else
                                     Castling_Validity.Black_King_Moved);
        Rook_Moved : Boolean := (if Color = White then
                                     Castling_Validity.White_Rook_Queenside_Moved
                                 else
                                     Castling_Validity.Black_Rook_Queenside_Moved);
    begin
        if (not King_Moved and not Rook_Moved
             and Board(a, Rank) = Rook
             and Board(b, Rank) = Empty
             and Board(c, Rank) = Empty
             and Board(d, Rank) = Empty
             and board(e, Rank) = King)
          and then not Is_King_Checked_Queenside(Board, Rank)
        then
            Board(a, Rank) := Empty;
            Board(b, Rank) := King;
            Board(c, Rank) := Rook;
            Board(d, Rank) := Empty;
            Board(e, Rank) := Empty;
        else
            Logs.Debug("Invalid castling");
        end if;
    end Castling_Queenside;
    

end Board.Castling;
