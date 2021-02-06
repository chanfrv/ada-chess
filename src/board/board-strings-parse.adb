with Gnat.Regexp; use GNAT.Regexp;
with Ada.Text_IO; use Ada.Text_IO;

with Logs;


package body Board.Strings.Parse is
    
    
    procedure Parser_Piece_Callback(Item : in String; Move : in out Move_t) is
    begin
        case Item(Item'First) is
            when 'k'|'K' =>
                Move.Piece := King;
            when 'q'|'Q' =>
                Move.Piece := Queen;
            when 'r'|'R' =>
                Move.Piece := Rook;
            when 'b'|'B' =>
                Move.Piece := Bishop;
            when 'n'|'N' =>
                Move.Piece := Knight;
            when others =>
                null;
        end case;
    end Parser_Piece_Callback;
    
    procedure Parser_From_File_Callback(Item : in String; Move : in out Move_t) is
        File : Character;
    begin
        File := Item(Item'First);
        Move.From_File := (False, File_t'Value("" & File));
    end Parser_From_File_Callback;
    
    procedure Parser_From_Rank_Callback(Item : in String; Move : in out Move_t) is
        Rank : Character;
    begin
        Rank := Item(Item'First);
        Move.From_Rank := (False, Rank_t'Value("" & Rank));
    end Parser_From_Rank_Callback;
    
    
    procedure Parser_Capture_Callback(Item : in String; Move : in out Move_t) is
    begin
        Move.Capture := True;
    end Parser_Capture_Callback;
    
    procedure Parser_To_Callback(Item : in String; Move : in out Move_t) is
        File : File_t := File_t'Value(Item(Item'First .. Item'First));
        Rank : Rank_t := Rank_t'Value(Item(Item'Last .. Item'Last));
    begin
        Move.To := (File => File, Rank => Rank);
    end Parser_To_Callback;
    
    procedure Parser_Promotion_Callback(Item : in String; Move : in out Move_t) is
    begin
        case Item(Item'First) is
            when 'q'|'Q' =>
                Move.Promotion := (IsEmpty => False, Value => Queen);
            when 'r'|'R' =>
                Move.Promotion := (IsEmpty => False, Value => Rook);
            when 'b'|'B' =>
                Move.Promotion := (IsEmpty => False, Value => Bishop);
            when 'n'|'N' =>
                Move.Promotion := (IsEmpty => False, Value => Knight);
            when others =>
                null;
        end case;
    end Parser_Promotion_Callback;
    
    procedure Parser_Status_Callback(Item : in String; Move : in out Move_t) is
    begin
        case Item(Item'First) is
            when '+' =>
                Move.Status := Check;
            when '#' =>
                Move.Status := Checkmate;
            when others =>
                null;
        end case;
    end Parser_Status_Callback;
    
    procedure Parser_Kingside_Callback(Item : in String; Move : in out Move_t) is
    begin
        Move := (Castling => Kingside);
    end Parser_Kingside_Callback;
    
    procedure Parser_Queenside_Callback(Item : in String; Move : in out Move_t) is
    begin
        Move := (Castling => Queenside);
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
        Logs.Inc_Indent;
        
        for Index_To in Parser_Nodes'Range loop
            Node_To := Parser_Nodes(Index_To);
            
            -- combined with the for loop => for each adjacent node
            if Parser_Adj_Matrix(Node, Index_To) then
                
                Logs.Debug("Traversing node '" & Node_To.Label.all & "'");
                
                -- if we are in a state
                if Node_To.Kind = State then
                    Regex := Compile(Node_To.Expr.all);
                    Logs.Debug("Trying to match string '" & Item & "'");
            
                    -- match string from grammar
                    End_Bound := Item'First + Node_To.Length - 1;
                    
                    if End_Bound <= Item'Last and then Match(Item(Item'First .. End_Bound), Regex) then
                        -- there is a match, call the callback
                        Logs.Debug("String '" & Item(Item'First .. End_Bound) & "' matched in node '" & Node_To.Label.all & "'");
                        Node_To.Eval(Item(Item'First .. End_Bound), Move);
                        New_First := Item'First + Node_To.Length;

                        -- recursively traverse the adjacent nodes
                        if Traverse_Rec(Item(New_First .. Item'Last), Move, Index_To) then
                            -- call unstacking
                            Logs.Dec_Indent;
                            return True;
                        else
                            -- this recursive call ended in an error, backtracking
                            Logs.Debug("Restoring move from '" & Image(Move) & "' to '" & Image(Tmp_Move) & "'");
                            Move := Tmp_Move;
                        end if;
                    end if;
                    
                elsif Node_To.Kind = Stop then
                    -- this node is the stop, success!
                    Logs.Dec_Indent;
                    return True;
                    
                end if;
                
            end if;
        end loop;
        
        -- we would arrive at this point if the whole string does not match the grammar
        Logs.Dec_Indent;
        return False;
    end Traverse_Rec;
    
    function Traverse(Item : in String; Move : out Move_t) return Boolean is
    begin
        Move := (Castling  => None,
                 Piece     => Pawn,
                 From_File => File_Empty,
                 From_Rank => Rank_Empty,
                 Capture   => False,
                 To        => (a, 1),
                 Promotion => Promotion_Empty,
                 Status    => Playing);
        
        Logs.Debug("Entering state machine");
        return Traverse_Rec(Item, Move, Parser_Nodes'First);
    end Traverse;
    
    
    procedure Parser_Pretty_Print
    is
        Node_From, Node_To : Parser_Node_t;
        
        First : Parser_Index_t := Parser_Index_t'Succ(Parser_Index_t'First);
        Last  : Parser_Index_t := Parser_Index_t'Pred(Parser_Index_t'Last);
    begin
        Put_Line("digraph Parser {");
        Put_Line("    rankdir=LR;");
        Put_Line("    node [ fontname = ""Helvetica"", shape=box, style=filled,bold ];");
        Put_Line("    "" START "" [fillcolor=black, shape=circle, label="""", width=0.25];");
        Put_Line("    "" END "" [fillcolor=black, shape=doublecircle, label="""", width=0.3];");
                        
        for State_From in Parser_Index_t loop
            
            for State_To in Parser_Index_t loop
                
                if Parser_Adj_Matrix(State_From, State_To) then
                                        
                    Node_From := Parser_Nodes(State_From);
                    Node_To   := Parser_Nodes(State_To);
                    
                    Put_Line("    "" " & Node_From.Label.all & " "" -> "" " & Node_To.Label.all & " "" [style=bold];");
                    
                end if;
            end loop;
            
        end loop;
        
        Put_Line("}");
    end Parser_Pretty_Print;
        
    
end Board.Strings.Parse;
