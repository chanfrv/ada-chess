package Board.Strings is
    
    
    function Image(File : File_t) return String;
    function Image(Rank : Rank_t) return String;
    
    function Image(Coordinates : in Coordinates_t) return String;
    
    function Image(File : in Opt_File_t) return String;
    function Image(Rank : in Opt_Rank_t) return String;

    -- Return the piece algebraic notation
    function Image(Piece : Piece_t) return String;
    
    function Image(Status : GameResult_t) return String;
    
    -- Converts the coordinates pair to an algebraic notation.
    function Image(Move : in Move_t) return String;    
    
    -- Return the content of a cell, the piece and its color.
    function Image(Cell : in Opt_Cell_t) return String;
    

end Board.Strings;
