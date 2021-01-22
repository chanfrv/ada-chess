package Board.Strings.Parse is
    
    
    subtype Parser_Index_t is Natural range 1 .. 9;
    
    type Node_Kind_t is (Start, Stop, State);
    
    type Parser_Callback_t is access function(Item : in String; Move : in out Move_t) return Natural;
    
    type Parser_Node_t(Kind : Node_Kind_t := State) is
        record
            Label : access constant String;
            
            case Kind is
                when State =>
                    Length : Positive;
                    Expr   : access constant String;
                    Eval   : Parser_Callback_t;
                when others =>
                    null;
            end case;
        end record;
    
    type Parser_Nodes_t is array (Parser_Index_t) of Parser_Node_t;
    
    type Parser_Adj_Matrix_t is array (Parser_Index_t, Parser_Index_t) of Boolean;
    
    
    function Parser_Piece_Callback    (Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_From_File_Callback(Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_From_Rank_Callback(Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_Capture_Callback  (Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_To_Callback       (Item : in String; Move : in out Move_t) return Natural
      with Pre => 2 <= Item'Length and Item'Length <= 2;
    function Parser_Promotion_Callback(Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_Status_Callback   (Item : in String; Move : in out Move_t) return Natural
      with Pre => 0 <= Item'Length and Item'Length <= 1;
    function Parser_Kingside_Callback (Item : in String; Move : in out Move_t) return Natural
      with Pre => 3 <= Item'Length and Item'Length <= 3;
    function Parser_Queenside_Callback(Item : in String; Move : in out Move_t) return Natural
      with Pre => 5 <= Item'Length and Item'Length <= 5;
    
    
    Parser_Piece_S     : aliased constant String := "[KQRBN]?";
    Parser_From_File_S : aliased constant String := "[a-h]?";
    Parser_From_Rank_S : aliased constant String := "[1-8]?";
    Parser_Capture_S   : aliased constant String := "x?";
    Parser_To_S        : aliased constant String := "[a-h][1-8]";
    Parser_Promotion_S : aliased constant String := "[QRBN]?";
    Parser_Status_S    : aliased constant String := "[+#]?";
    Parser_Kingside_S  : aliased constant String := "0-0";
    Parser_Queenside_S : aliased constant String := "0-0-0";
    
    Parser_Start_L     : aliased constant String := "START";
    Parser_Stop_L      : aliased constant String := "END";
    Parser_Piece_L     : aliased constant String := "Piece '" & Parser_Piece_S & "'";
    Parser_From_File_L : aliased constant String := "From(Rank) '" & Parser_From_File_S & "'";
    Parser_From_Rank_L : aliased constant String := "From(File) '" & Parser_From_Rank_S & "'";
    Parser_Capture_L   : aliased constant String := "Capture '" & Parser_Capture_S & "'";
    Parser_To_L        : aliased constant String := "To '" & Parser_To_S & "'";
    Parser_Promotion_L : aliased constant String := "Promotion '" & Parser_Promotion_S & "'";
    Parser_Status_L    : aliased constant String := "Status '" & Parser_Status_S & "'";
    Parser_Kingside_L  : aliased constant String := "Kingside '" & Parser_Kingside_S & "'";
    Parser_Queenside_L : aliased constant String := "Queenside '" & Parser_Queenside_S & "'";
    
    
    Parser_Start     : constant Parser_Node_t := (Kind => Start, Label => Parser_Start_L'Access);
    Parser_Stop      : constant Parser_Node_t := (Kind => Stop, Label => Parser_Stop_L'Access);    
    
    Parser_Piece     : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Piece_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_Piece_S'Access,
                                                  Eval => Parser_Piece_Callback'Access);
    Parser_From_File : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_From_File_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_From_File_S'Access,
                                                  Eval => Parser_From_File_Callback'Access);
    Parser_From_Rank : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_From_Rank_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_From_Rank_S'Access,
                                                  Eval => Parser_From_Rank_Callback'Access);
    Parser_Capture   : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Capture_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_Capture_S'Access,
                                                  Eval => Parser_Capture_Callback'Access);
    Parser_To        : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_To_L'Access,
                                                  Length => 2,
                                                  Expr => Parser_To_S'Access,
                                                  Eval => Parser_To_Callback'Access);
    Parser_Promotion : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Promotion_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_Promotion_S'Access,
                                                  Eval => Parser_Promotion_Callback'Access);
    Parser_Status    : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Status_L'Access,
                                                  Length => 1,
                                                  Expr => Parser_Status_S'Access,
                                                  Eval => Parser_Status_Callback'Access);
    Parser_Kingside  : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Kingside_L'Access,
                                                  Length => 3,
                                                  Expr => Parser_Kingside_S'Access,
                                                  Eval => Parser_Kingside_Callback'Access);
    Parser_Queenside : constant Parser_Node_t := (Kind => State,
                                                  Label => Parser_Queenside_L'Access,
                                                  Length => 5,
                                                  Expr => Parser_Queenside_S'Access,
                                                  Eval => Parser_Queenside_Callback'Access);
    
    Parser_Nodes : constant Parser_Nodes_t := (Parser_Start,
                                               Parser_Piece,
                                               Parser_From_File,
                                               Parser_From_Rank,
                                               Parser_Capture,
                                               Parser_To,
                                               Parser_Promotion,
                                               Parser_Status,
                                               Parser_Stop);
    
    Parser_Adj_Matrix : constant Parser_Adj_Matrix_t :=
    (
        (False, True,  True,  True,  True,  True,  False, False, False), -- start     -> piece | from_file | from_rank | capture | to
        (False, False, True,  True,  True,  True,  False, False, False), -- piece     -> from_file | from_rank | capture | to
        (False, False, False, True,  True,  True,  False, False, False), -- from_file -> from_rank | capture | to
        (False, False, False, False, True,  True,  False, False, False), -- from_rank -> capture | to
        (False, False, False, False, False, True,  False, False, False), -- capture   -> to
        (False, False, False, False, False, False, True,  True,  True),  -- to        -> promotion | status | stop
        (False, False, False, False, False, False, False, True,  True),  -- promotion -> status | stop
        (False, False, False, False, False, False, False, False, True),  -- status    -> stop
        (False, False, False, False, False, False, False, False, False)  -- stop
    );
    
    
    function Traverse(Item : in String; Move : out Move_t) return Boolean
      with Pre => Item'Length >= 1;
    
    
    procedure Parser_Pretty_Print;
    

end Board.Strings.Parse;
