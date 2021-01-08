package Board.Parse is

   
    function Image(Coordinates : in Coordinates_t) return String;
    
    function Image(Coordinates : in Disambiguating_Coordinates_t) return String;
    
    -- Converts the coordinates pair to an algebraic notation.
    function Image(Move : in Move_t) return String;
        
    function Image(Cell : in Cell_t) return String;
    
    
    procedure Pretty_Print(Board : in Board_t);

    
    -- Converts an algebraic string to a coordinates pair.
    function Value(Move_Str : in String) return Move_t
      with Pre => Move_Str'Length >= 1;
    

end Board.Parse;
