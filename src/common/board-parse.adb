with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Board.Parse is

   
    function Image(Coordinates : Coordinates_t) return String is
    begin
        return To_Lower(Coordinates.File'Image) & Trim(Coordinates.Rank'Image, Ada.Strings.Left);
    end Image;

    function Image(Coordinates : Disambiguating_Coordinates_t) return String is
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

    
    function Image(Move : Move_t) return String is
    begin
        return To_Lower(Move.Piece'Image)
          & (if Move.Capture then " [capture]" else "")
          & Image(Move.From)
          & " "
          & Image(Move.To);
    end Image;
    
    
    function Image(Cell : Cell_t) return String is
    begin
        case Cell.IsEmpty is
            when True =>
                return "empty";
            when False =>
                return To_Lower(Cell.Color'Image & " " & Cell.Piece'Image);
        end case;
    end Image;
    
    
    procedure GetPieceDisambiguity(Move_Str : String; Move : in out Move_t) is
        Index : Positive;
        Curr : Character;

        Has_Coords : Has_Coordinates_t := Has_None;
        Rank : Rank_t;
        File : File_t;
    begin        
        Index := Move_Str'Last;
        Curr := Move_Str(Index);

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

    
    procedure GetCapture(Move_Str : String; Move : in out Move_t) is
    begin        
        if Move_Str'Length = 0 then
            Move.Capture := False;
        elsif Move_Str(Move_Str'Last) = 'x' then
            Move.Capture := True;
            if Move_Str'Length > 1 then
                GetPieceDisambiguity(Move_Str(Move_Str'First .. Move_Str'Last - 1), move);
            end if;
        else
            Move.Capture := False;
            GetPieceDisambiguity(Move_Str, move);
        end if;
    end GetCapture;

    
    function Value(Move_Str : String) return Move_t is
        Move : Move_t;

        File_C : Character;
        Rank_C : Character;
    begin
        Put_Line("[PARSE] Value string: '" & Move_Str & "'");
        
        -- Init
        Move.Piece := Pawn;
        Move.Capture := False;
        Move.From := (Has => Has_None);
        
        File_C := Move_Str(Move_Str'Last - 1);
        case File_C is
            when 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>
                Move.To.File := File_t'Value("" & File_C);
            when others =>
                Put_Line("Invalid destination file");
        end case;

        Rank_C := Move_Str(Move_Str'Last);
        case Rank_C is
            when '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>
                Move.To.Rank := Rank_t'Value("" & Rank_C);
            when others =>
                Put_Line("Invalid destination rank");
        end case;
        
        GetCapture(Move_Str(Move_Str'First .. Move_Str'Last - 2), Move);

        return Move;
    end Value;


end Board.Parse;
