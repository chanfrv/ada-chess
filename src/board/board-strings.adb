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

    function Image(File : in Opt_File_t) return String is
    begin
        return (case File.IsEmpty is
                    when True => "",
                    when False => Image(File.Value));
    end Image;
    
    function Image(Rank : in Opt_Rank_t) return String is
    begin
        return (case Rank.IsEmpty is
                    when True => "",
                    when False => Image(Rank.Value));
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
    
    
    function Image(Status : GameResult_t) return String is
    begin
        return (case Status is
                    when Check     => "+",
                    when Checkmate => "#",
                    when others    => "");
    end Image;
    
        
    function Image(Move : in Move_t) return String is
    begin
        return Image(Move.Piece)
          & Image(Move.From_File) & Image(Move.From_Rank)
          & (if Move.Capture then "x" else "")
          & Image(Move.To)
          & (if not Move.Promotion.IsEmpty then Image(Move.Promotion.Value) else "")
          & Image(Move.Status);
    end Image;
        
    
    function Image(Cell : in Opt_Cell_t) return String is
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
