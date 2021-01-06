with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Board.Parse is

   
    function Image(Coordinates : in Coordinates_t) return String is
    begin
        return To_Lower(Coordinates.File'Image) & Trim(Coordinates.Rank'Image, Ada.Strings.Left);
    end Image;

    function Image(Coordinates : in Disambiguating_Coordinates_t) return String is
    begin
        case Coordinates.Has is
            when Has_None =>
                return "";
            when Has_File =>
                return To_Lower(Coordinates.File'Image);
            when Has_Rank =>
                return Trim(Coordinates.Rank'Image, Ada.Strings.Left);
            when Has_Both =>
                return Image(Coordinates.Coordinates);
        end case;
    end Image;

    
    function Image(Move : in Move_t) return String is
    begin
        return To_Lower(Move.Piece'Image)
          & (if Move.Capture then " [capture]" else "")
          & Image(Move.From)
          & " "
          & Image(Move.To);
    end Image;
        
    
    function Image(Cell : in Cell_t) return String is
    begin
        case Cell.IsEmpty is
            when True =>
                -- return "";
                return "  ";
            when False =>
                --return To_Lower(Cell.Color'Image & " " & Cell.Piece'Image);
                return (case Cell.Color is
                            when White => "W",
                            when Black => "B")
                     & (case Cell.Piece is
                            when King   => "K",
                            when Queen  => "Q",
                            when Rook   => "R",
                            when Bishop => "B",
                            when Knight => "N",
                            when Pawn   => "p");
        end case;
    end Image;
    
    
    procedure Pretty_Print(Board : in Board_t)
    is
        Cell : Cell_t;
    begin
        for File in a .. h loop
            if File = a then
                Put_Line("    1  2  3  4  5  6  7  8");
            end if;
            
            for Rank in 1 .. 8 loop
                if Rank = 1 then
                    Put(To_Lower(File'Image) & "|");
                end if;
                
                Cell := Board(File, Rank);
                Put(" " & Image(Cell));
            end loop;
            New_Line;
        end loop;
    end Pretty_Print;
    
    
    procedure GetPieceDisambiguity(Move_Str : in String; Move : in out Move_t) is
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
            when '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>
                Has_Coords := Has_Rank;
                Rank := Rank_t'Value("" & Curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Move_Str'Last - 1);
                end if;

            when others =>
                null;
        end case;

        -- Origin file
        case Curr is
            when 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>
                Has_Coords := (if Has_Coords = Has_Rank then Has_Both else Has_File);
                File := File_t'Value("" & curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Move_Str'Last - 1);
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
    
    procedure GetPosition(Move_Str : in String; Move : in out Move_t) is
        File_C : Character;
        Rank_C : Character;
    begin
        -- File
        File_C := Move_Str(Move_Str'Last - 1);
        case File_C is
            when 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>
                Move.To.File := File_t'Value("" & File_C);
            when others =>
                Put_Line("Invalid destination file");
        end case;

        -- Rank
        Rank_C := Move_Str(Move_Str'Last);
        case Rank_C is
            when '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>
                Move.To.Rank := Rank_t'Value("" & Rank_C);
            when others =>
                Put_Line("Invalid destination rank");
        end case;
        
        -- Capture, piece and disambiguity
        GetCapture(Move_Str(Move_Str'First .. Move_Str'Last - 2), Move);
    
    end GetPosition;
    
    
    function Value(Move_Str : in String) return Move_t is
        Move : Move_t;
        
        Char  : Character := Move_Str(Move_Str'Last);
    begin        
        -- Init
        Move.Piece := Pawn;
        Move.Capture := False;
        Move.From := (Has => Has_None);
        Move.Promotion := Pawn;
        
        -- Promotion handling
        case Char is
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
                -- No promotion
                GetPosition(Move_Str(Move_Str'First .. Move_Str'Last), Move);
        end case;
        
        return Move;
    end Value;


end Board.Parse;
