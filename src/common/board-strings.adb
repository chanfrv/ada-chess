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
    
    
    procedure GetPieceDisambiguity(Move_Str : in String; Move : in out Move_t)
      with Pre => Move_Str'Length >= 1
    is
        Index : Positive;
        Curr : Character;

        Has_Coords : Has_Coordinates_t := Has_None;
        Rank : Rank_t;
        File : File_t;
    begin        
        Index := Move_Str'Last;
        Curr := Move_Str(Index);

        -- Origin rank
        case Curr is
            when '1' .. '8' =>
                Has_Coords := Has_Rank;
                Rank := Rank_t'Value("" & Curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Index);
                end if;

            when others =>
                null;
        end case;

        -- Origin file
        case Curr is
            when 'a' .. 'h' =>
                Has_Coords := (if Has_Coords = Has_Rank then Has_Both else Has_File);
                File := File_t'Value("" & curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Index);
                end if;

            when others =>
                null;
        end case;

        -- Build the disambiguity coordinates
        case Has_Coords is
            when Has_None =>
                Move.From := (Has => Has_None);
            when Has_Rank =>
                Move.From := (Has => Has_Rank, Rank => Rank);
            when Has_File =>
                Move.From := (Has => Has_File, File => File);
            when Has_Both =>
                Move.From := (Has => Has_Both, Coordinates => (Rank => Rank, File => File));
        end case;

        -- Get the piece
        case Curr is
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
    end GetPieceDisambiguity;

    
    procedure GetCapture(Move_Str : in String; Move : in out Move_t) is
    begin
        -- No more info
        if Move_Str'Length = 0 then
            Move.Capture := False;
        -- Capture
        elsif Move_Str(Move_Str'Last) = 'x' then
            Move.Capture := True;
            -- Disambiguity
            if Move_Str'Length > 1 then
                GetPieceDisambiguity(Move_Str(Move_Str'First .. Move_Str'Last - 1), move);
            end if;
        -- Disambiguity
        else
            Move.Capture := False;
            GetPieceDisambiguity(Move_Str, move);
        end if;
    end GetCapture;
    
    procedure GetPosition(Move_Str : in String; Move : in out Move_t)
      with Pre => Move_Str'Length >= 2
    is
        File_C : Character;
        Rank_C : Character;
    begin
        -- File
        File_C := Move_Str(Move_Str'Last - 1);
        case File_C is
            when 'a' .. 'h' =>
                Move.To.File := File_t'Value("" & File_C);
            when others =>
                Logs.Debug("Invalid destination file");
        end case;

        -- Rank
        Rank_C := Move_Str(Move_Str'Last);
        case Rank_C is
            when '1' .. '8' =>
                Move.To.Rank := Rank_t'Value("" & Rank_C);
            when others =>
                Logs.Debug("Invalid destination rank");
        end case;
        
        -- Capture, piece and disambiguity
        GetCapture(Move_Str(Move_Str'First .. Move_Str'Last - 2), Move);
    
    end GetPosition;
    
    
    function Parse(Move_Str : in String) return Move_t is
        Move : Move_t := (Castling  => None,
                          Piece     => Pawn,
                          From      => (Has => Has_None),
                          Capture   => False,
                          To        => (a, 1),
                          Promotion => Pawn);
        
        Char : Character := Move_Str(Move_Str'Last);
    begin
        case Char is
            -- Promotion handling
            when 'Q'|'R'|'B'|'N' =>
                case Char is
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
                -- Has promotion
                GetPosition(Move_Str(Move_Str'First .. Move_Str'Last - 1), Move);
                
            when others =>
                -- Kingside castling handling
                if Move_Str'Length = 3
                  and then Move_Str(Move_Str'First .. Move_Str'Last) = "0-0" then
                    Move := (Castling => Kingside);
                    
                -- Queenside castling handling
                elsif Move_Str'Length = 5
                  and then Move_Str(Move_Str'First .. Move_Str'Last) = "0-0-0" then
                    Move := (Castling => Queenside);
                    
                -- No promotion or castling
                else
                    GetPosition(Move_Str(Move_Str'First .. Move_Str'Last), Move);
                end if;
        end case;

        return Move;
    end Parse;


end Board.Strings;
