package Board.Strings is

    function Image(File : File_t) return String;
    function Image(Rank : Rank_t) return String;
    
    function Image(Coordinates : in Coordinates_t) return String;    
    function Image(Coordinates : in Disambiguating_Coordinates_t) return String;
    
    -- Return the piece algebraic notation
    function Image(Piece : Piece_t) return String;
    
    -- Converts the coordinates pair to an algebraic notation.
    function Image(Move : in Move_t) return String;
    
    
    -- Return the content of a cell, the piece and its color.
    function Image(Cell : in Cell_t) return String;
        
    -- Pretty prints the board on the standard output.
    procedure Pretty_Print(Board : in Board_t);

    
    -- Converts an algebraic string to a coordinates pair.
    function Parse(Move_Str : in String) return Move_t
      with Pre => Move_Str'Length >= 1;
    

end Board.Strings;
