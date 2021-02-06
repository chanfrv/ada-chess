package Board.Strings.Parse is
    
    
    subtype Parser_Index_t is Natural range 1 .. 11;
    
    type Node_Kind_t is (Start, Stop, State);
    
    type Parser_Callback_t is access procedure(Item : in String; Move : in out Move_t);
    
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
    
    
    procedure Parser_Piece_Callback    (Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_From_File_Callback(Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_From_Rank_Callback(Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_Capture_Callback  (Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_To_Callback       (Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 2;
    procedure Parser_Promotion_Callback(Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_Status_Callback   (Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 1;
    procedure Parser_Kingside_Callback (Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 3;
    procedure Parser_Queenside_Callback(Item : in String; Move : in out Move_t)
      with Pre => Item'Length = 2;
    
    
    Parser_Piece_S     : aliased constant String := "[kqrbnKQRBN]";
    Parser_From_File_S : aliased constant String := "[a-hA-H]";
    Parser_From_Rank_S : aliased constant String := "[1-8]";
    Parser_Capture_S   : aliased constant String := "[xX]";
    Parser_To_S        : aliased constant String := "[a-hA-H][1-8]";
    Parser_Promotion_S : aliased constant String := "[qrbnQRBN]";
    Parser_Status_S    : aliased constant String := "[+#]";
    Parser_Kingside_S  : aliased constant String := "0-0";
    Parser_Queenside_S : aliased constant String := "-0";
    
    Parser_Start_L     : aliased constant String := "START";
    Parser_Stop_L      : aliased constant String := "END";
    Parser_Piece_L     : aliased constant String := "Piece";
    Parser_From_File_L : aliased constant String := "From (Rank)";
    Parser_From_Rank_L : aliased constant String := "From (File)";
    Parser_Capture_L   : aliased constant String := "Capture";
    Parser_To_L        : aliased constant String := "To";
    Parser_Promotion_L : aliased constant String := "Promotion";
    Parser_Status_L    : aliased constant String := "Status";
    Parser_Kingside_L  : aliased constant String := "Kingside";
    Parser_Queenside_L : aliased constant String := "Queenside";
    
    
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
                                                  Length => 2,
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
                                               Parser_Kingside,
                                               Parser_Queenside,
                                               Parser_Stop);
    
    Parser_Adj_Matrix : constant Parser_Adj_Matrix_t :=
    (
        -- start     -> piece | from_file | from_rank | capture | to | kingside
        (False, True,  True,  True,  True,  True,  False, False, True,  False, False),
        -- piece     -> from_file | from_rank | capture | to
        (False, False, True,  True,  True,  True,  False, False, False, False, False),
        -- from_file -> from_rank | capture | to
        (False, False, False, True,  True,  True,  False, False, False, False, False),
        -- from_rank -> capture | to
        (False, False, False, False, True,  True,  False, False, False, False, False),
        -- capture   -> to
        (False, False, False, False, False, True,  False, False, False, False, False),
        -- to        -> promotion | status | stop
        (False, False, False, False, False, False, True,  True,  False, False, True),
        -- promotion -> status | stop
        (False, False, False, False, False, False, False, True,  False, False, True),
        -- status    -> stop
        (False, False, False, False, False, False, False, False, False, False, True),
        -- kingside  -> queenside | stop
        (False, False, False, False, False, False, False, False, False, True,  True),
        -- queenside -> stop
        (False, False, False, False, False, False, False, False, False, False, True),
        -- stop
        (False, False, False, False, False, False, False, False, False, False, False)
    );
    
    
    function Traverse(Item : in String; Move : out Move_t) return Boolean
      with Pre => Item'Length >= 1;
    
    
    procedure Parser_Pretty_Print;
    

end Board.Strings.Parse;
