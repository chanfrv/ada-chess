with Gnat.Regexp; use GNAT.Regexp;
with Ada.Text_IO; use Ada.Text_IO;

with Logs;


package body Board.Strings.Parse is
    
    
    function Parser_Piece_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        if Item'Length = 0 then
            Move.Piece := Pawn;
            return 0;
        else
            case Item(Item'First) is
                when 'K' =>
                    Move.Piece := King;
                when 'Q' =>
                    Move.Piece := Queen;
                when 'R' =>
                    Move.Piece := Rook;
                when 'B' =>
                    Move.Piece := Bishop;
                when 'N' =>
                    Move.Piece := Knight;
                when others =>
                    null;
            end case;
            return 1;
        end if;
    end Parser_Piece_Callback;
    
    function Parser_From_Callback(Item : in String; Move : in out Move_t) return Natural
    is                
        Index : Positive;
        Curr : Character;
        
        File : File_t;
        Rank : Rank_t;
    begin
        if Item'Length = 0 then
            Move.From := (Has => Has_None);
            return 0;
            
        else
            index := Item'First;
            Curr  := Item(Index);
        
            -- Origin file
            case Curr is
                when 'a' .. 'h' =>
                    File := File_t'Value("" & Curr);
                    
                    Move.From := (Has => Has_File, File => File);
                    
                    if Item'Length > 1 then
                        Index := Index + 1;
                        Curr := Item(Index);
                    end if;
                    
                when others =>
                    null;
            end case;
            
            -- Origin rank
            case Curr is
                when '1' .. '8' =>
                    Rank := Rank_t'Value("" & Curr);
                    
                    if Move.From.Has = Has_None then
                        Move.From := (Has => Has_Rank, Rank => Rank);
                    else
                        Move.From := (Has => Has_Both, Coordinates => (File, Rank));
                    end if;
                    
                when others =>
                    null;
            end case;
        
            return Index - 1;
        end if;

    end Parser_From_Callback;
    
    function Parser_Capture_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        if Item'Length = 0 then
            Move.Capture := False;
            return 0;
        else
            Move.Capture := True;
            return 1;
        end if;
    end Parser_Capture_Callback;
    
    function Parser_To_Callback(Item : in String; Move : in out Move_t) return Natural
    is
        File : File_t := File_t'Value(Item(Item'First .. Item'First));
        Rank : Rank_t := Rank_t'Value(Item(Item'Last .. Item'Last));
    begin
        Move.To := (File => File, Rank => Rank);
        return 2;
    end Parser_To_Callback;
    
    function Parser_Promotion_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        if Item'Length = 0 then
            Move.Promotion := Pawn;
            return 0;
        else
            case Item(Item'First) is
                when 'Q' =>
                    Move.Promotion := Queen;
                when 'R' =>
                    Move.Promotion := Rook;
                when 'B' =>
                    Move.Promotion := Bishop;
                when 'N' =>
                    Move.Promotion := Knight;
                when others =>
                    null;
            end case;
            return 1;
        end if;
    end Parser_Promotion_Callback;
    
    function Parser_Status_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        if Item'Length = 0 then
            Move.Status := Playing;
            return 0;
        else
            case Item(Item'First) is
                when '+' =>
                    Move.Status := Check;
                when '#' =>
                    Move.Status := Checkmate;
                when others =>
                    null;
            end case;
            return 1;
        end if;
    end Parser_Status_Callback;
    
    function Parser_Kingside_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        Move := (Castling => Kingside);
        return 3;
    end Parser_Kingside_Callback;
    
    function Parser_Queenside_Callback(Item : in String; Move : in out Move_t) return Natural is
    begin
        Move := (Castling => Queenside);
        return 5;
    end Parser_Queenside_Callback;
    
    
    function Traverse_Rec(Item : in String; Move : in out Move_t; Node : in Parser_Index_t) return Boolean
    is
        Node_To   : Parser_Node_t;
        End_Bound : Positive;
        New_First : Positive;
        Regex     : Regexp;
        Tmp_Move  : Move_t := Move;
    begin
        -- Enter graph node
        Logs.Debug("Current state: '" & Parser_Nodes(Node).Label.all & "', string: '" & Item & "'");
        Logs.IncIndent;
        
        for Index_To in Parser_Nodes'Range loop
            Node_To := Parser_Nodes(Index_To);
            
            -- combined with the for loop => for each adjacent node
            if Parser_Adj_Matrix(Node, Index_To) then
                
                Logs.Debug("Traversing node '" & Node_To.Label.all & "'");
                
                -- if we are in a state
                if Node_To.Kind = State then
                    Regex := Compile(Node_To.Expr.all);
                    Logs.Debug("Trying to match string '" & Item & "' from " & Item'First'Image & " to " & Node_To.Length'Image);
            
                    -- match string from grammar
                    End_Bound := Positive'Min(Item'Last, Item'First + Node_To.Length - 1);
                    
                    if Match(Item(Item'First .. End_Bound), Regex) then
                        -- there is a match, call the callback
                        Logs.Debug("String '" & Item(Item'First .. End_Bound) & "' matched in node '" & Node_To.Label.all & "'");
                        New_First := Item'First + Node_To.Eval(Item, Move);

                        -- recursively traverse the adjacent nodes
                        if Traverse_Rec(Item(New_First .. Item'Last), Move, Index_To) then
                            -- call unstacking
                            Logs.DecIndent;
                            return True;
                        else
                            -- this recursive call ended in an error, backtracking
                            Logs.Debug("Restoring move from '" & Image(Move) & "' to '" & Image(Tmp_Move) & "'");
                            Move := Tmp_Move;
                        end if;
                    end if;
                    
                elsif Node_To.Kind = Stop then
                    -- this node is the stop, success!
                    Logs.DecIndent;
                    return True;
                    
                end if;
                
            end if;
        end loop;
        
        -- we would arrive at this point if the whole string does not match the grammar
        Logs.DecIndent;
        return False;
    end Traverse_Rec;
    
    function Traverse(Item : in String) return Move_t is
        Move : Move_t := (Castling  => None,
                          Piece     => Pawn,
                          From      => (Has => Has_None),
                          Capture   => False,
                          To        => (a, 1),
                          Promotion => Pawn,
                          Status    => Playing);
        
        Success : Boolean;
    begin
        Logs.Debug("Entering state machine");
        Success := Traverse_Rec(Item, Move, Parser_Nodes'First);
        Logs.Debug("Parsing status: " & Success'Image);
        
        return Move;
    end Traverse;
    
    
    procedure Parser_Pretty_Print
    is
        Node_From, Node_To : Parser_Node_t;
    begin
        Put_Line("digraph Parser {");
        
        for State_From in Parser_Index_t loop
            
            for State_To in Parser_Index_t loop
                
                if Parser_Adj_Matrix(State_From, State_To) then
                    
                    Node_From := Parser_Nodes(State_From);
                    Node_To   := Parser_Nodes(State_To);
                    
                    Put_Line(("    """ & Node_From.Label.all & """ -> """ & Node_To.Label.all & """"));
                    
                end if;
            end loop;
            
        end loop;
        
        Put_Line("}");
    end Parser_Pretty_Print;
        
    
end Board.Strings.Parse;
