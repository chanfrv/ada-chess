with Ada.Text_IO; use Ada.Text_IO;


package body Board.Castling is

   
    procedure Kingside_Castling(Board : in out Board_t; Color : in Color_t)
    is
        Rank : Rank_t := (if Color = White then 1 else 8);
        King : Cell_t := (if Color = White then WKing else BKing);
        Rook : Cell_t := (if Color = White then WRook else BRook);
    begin
        if Board(e, Rank) = King and Board(f, Rank) = Empty
          and Board(g, Rank) = Empty and Board(h, Rank) = Rook
        then
            Board(e, Rank) := Empty;
            Board(f, Rank) := Rook;
            Board(g, Rank) := King;
            Board(h, Rank) := Empty;
        else
            Put_Line("Invalid castling");
        end if;
    end Kingside_Castling;
    
    
    procedure Queenside_Castling(Board : in out Board_t; Color : in Color_t)
    is
        Rank : Rank_t := (if Color = White then 1 else 8);
        King : Cell_t := (if Color = White then WKing else BKing);
        Rook : Cell_t := (if Color = White then WRook else BRook);        
    begin
        if Board(a, Rank) = Rook and Board(b, Rank) = Empty
          and Board(c, Rank) = Empty and Board(d, Rank) = Empty
          and board(e, Rank) = King
        then
            Board(a, Rank) := Empty;
            Board(b, Rank) := King;
            Board(c, Rank) := Rook;
            Board(d, Rank) := Empty;
            Board(e, Rank) := Empty;
        else
            Put_Line("Invalid castling");
        end if;
    end Queenside_Castling;
    

end Board.Castling;
