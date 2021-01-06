package Board.Parse is

   
    function Image(Coordinates : Coordinates_t) return String;
    
    function Image(Coordinates : Disambiguating_Coordinates_t) return String;
    
    -- Converts the coordinates pair to an algebraic notation.
    function Image(Move : Move_t) return String;
    
    function Image(Cell : Cell_t) return String;
    
    
    -- Converts an algebraic string to a coordinates pair.
    function Value(Move_Str : String) return Move_t;


end Board.Parse;
