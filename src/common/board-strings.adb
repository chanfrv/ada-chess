with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Logs;


package body Board.Strings is

   
    function Image(File : File_t) return String is
    begin
        return To_Lower(File'Image);
    end Image;
    
    function Image(Rank : Rank_t) return String is
    begin
        return Trim(Rank'Image, Ada.Strings.Left);
    end Image;
    
    function Image(Coordinates : in Coordinates_t) return String is
    begin
        return Image(Coordinates.File) & Image(Coordinates.Rank);
    end Image;

    function Image(Coordinates : in Disambiguating_Coordinates_t) return String is
    begin
        case Coordinates.Has is
            when Has_None =>
                return "";
            when Has_File =>
                return Image(Coordinates.File);
            when Has_Rank =>
                return Image(Coordinates.Rank);
            when Has_Both =>
                return Image(Coordinates.Coordinates);
        end case;
    end Image;
    
    function Image(Piece : Piece_t) return String is
    begin
        return (case Piece is
                    when King   => "K",
                    when Queen  => "Q",
                    when Rook   => "R",
                    when Bishop => "B",
                    when Knight => "N",
                    when Pawn   => "");        
    end Image;
        
    function Image(Move : in Move_t) return String is
    begin
        return Image(Move.Piece)
          & Image(Move.From)
          & (if Move.Capture then "x" else "")
          & Image(Move.To)
          & (if Move.Promotion /= Pawn then Image(Move.Promotion) else "");
    end Image;
        
    
    function Image(Cell : in Cell_t) return String is
    begin
        return (if    Cell = WKing   then "♔"
                elsif Cell = WQueen  then "♕"
                elsif Cell = WRook   then "♖"
                elsif Cell = WBishop then "♗"
                elsif Cell = WKnight then "♘"
                elsif Cell = WPawn   then "♙"
                elsif Cell = BKing   then "♚"
                elsif Cell = BQueen  then "♛"
                elsif Cell = BRook   then "♜"
                elsif Cell = BBishop then "♝"
                elsif Cell = BKnight then "♞"
                elsif Cell = BPawn   then "♟︎"
                else " ");
    end Image;


end Board.Strings;
