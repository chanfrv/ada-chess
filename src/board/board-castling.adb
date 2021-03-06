with Logs;


package body Board.Castling is

   
    procedure Castling_Unregister(Board : in Board_t; From : in Coordinates_t)
    is
        Cell : Opt_Cell_t := Board(From.File, From.Rank);        
        Side : Castling_t := (if From.File <= d then Queenside else Kingside);
    begin
        -- White king
        if Cell = WKing then
            White_King_Moved := True;
        
        -- White rook
        elsif Cell = WRook then
            -- Queenside rook
            if Side = Queenside then
                White_Rook_Queenside_Moved := True;
            -- Kingside rook
            else
                White_Rook_Kingside_Moved := True;
            end if;
        
        -- Black king
        elsif Cell = BKing then
            Black_King_Moved := True;
        
        -- Black rook
        elsif Cell = BRook then
            -- Queenside rook
            if Side = Queenside then
                Black_Rook_Queenside_Moved := True;
            -- Kingside rook
            else
                Black_Rook_Kingside_Moved := True;
            end if;
        end if;
    end Castling_Unregister;
    
  
    function Castling_Kingside(Board : in out Board_t; Color : in Color_t) return MoveResult_t
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
        
        King : Opt_Cell_t := (if Color = White then WKing else BKing);
        Rook : Opt_Cell_t := (if Color = White then WRook else BRook);
        
        King_Moved : Boolean := (if Color = White then
                                     White_King_Moved
                                 else
                                     Black_King_Moved);
        Rook_Moved : Boolean := (if Color = White
                                 then White_Rook_Kingside_Moved
                                 else Black_Rook_Kingside_Moved);        
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
            return Valid_Move;
        else
            Logs.Debug("Invalid castling");
            return Invalid_Move;
        end if;
    end Castling_Kingside;
    
    
    function Castling_Queenside(Board : in out Board_t; Color : in Color_t) return MoveResult_t
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
        
        King : Opt_Cell_t := (if Color = White then WKing else BKing);
        Rook : Opt_Cell_t := (if Color = White then WRook else BRook);
        
        King_Moved : Boolean := (if Color = White
                                 then White_King_Moved
                                 else Black_King_Moved);
        Rook_Moved : Boolean := (if Color = White
                                 then White_Rook_Queenside_Moved
                                 else Black_Rook_Queenside_Moved);
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
            return Valid_Move;
        else
            Logs.Debug("Invalid castling");
            return Invalid_Move;
        end if;
    end Castling_Queenside;
    

end Board.Castling;
