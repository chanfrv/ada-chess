with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;


package body Board is

    function Image(Coordinates : Coordinates_t) return String is
    begin
        return To_Lower(Coordinates.File'Image) & Coordinates.Rank'Image;
    end Image;

    function Image(Coordinates : Disambiguating_Coordinates_t) return String is
    begin
        case Coordinates.Has is
            when Has_None =>
                return "";
            when Has_File =>
                return To_Lower(Coordinates.File'Image);
            when Has_Rank =>
                return Coordinates.Rank'Image;
            when Has_Both =>
                return Image(Coordinates.Coordinates);
        end case;
    end Image;

    function Image(Move : Move_t) return String is
    begin
        return To_Lower(Move.Piece'Image) & (if Move.Capture then "x" else " ") & Image(Move.From) & Image(Move.To);
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
        Put_Line("Disambiguity substring: '" & Move_Str & "'");

        Index := Move_Str'Last;
        Curr := Move_Str(Index);
        Move.Piece := Pawn;
        Move.From := (Has => Has_None);

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
        Put_Line("Capture substring: '" & Move_Str & "'");

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
        Put_Line("Movement string: '" & Move_Str & "'");

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


    function GetCoordinates(File : Integer; Rank : Integer) return Disambiguating_Coordinates_t is
    begin
        if File > File_t'Pos(File_t'Last) or File < File_t'Pos(File_t'First) then
            return (Has => Has_None);
        end if;
        if Rank > Rank_t'Pos(Rank_t'Last) or Rank < Rank_t'Pos(Rank_t'First) then
            return (Has => Has_None);
        end if;
        return (Has_Both, (File_t'Val(File), Rank_t'Val(Rank)));
    end GetCoordinates;


    function KingTest(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t is
        FileInt : Integer := File_t'Pos(CurrMove.From.File);
        RankInt : Integer := Rank_t'Pos(CurrMove.From.Rank);
        type Valid_Coord_Array is array (Integer) of Disambiguating_Coordinates_t;
        ValidArr : Valid_Coord_Array := (
            GetCoordinates(FileInt + 1, RankInt + 1),
            GetCoordinates(FileInt + 1, RankInt),
            GetCoordinates(FileInt + 1, RankInt - 1),
            GetCoordinates(FileInt, RankInt + 1),
            GetCoordinates(FileInt, RankInt - 1),
            GetCoordinates(FileInt - 1, RankInt + 1),
            GetCoordinates(FileInt - 1, RankInt),
            GetCoordinates(FileInt - 1, RankInt - 1)
            );
        TmpPiece : Cell_t;
    begin
        if ValidArr(1) /= (Has => Has_None) then
            return Invalid_Move;
        end if;
        for I in ValidArr'Range loop
            if ValidArr(I) /= (Has => Has_None) then
                TmpPiece := Board(ValidArr(I).File, ValidArr(I).Rank);
                if TmpPiece.Color = CurrPlayerColor then
                    ValidArr(I) := (Has => Has_None);
                end if;
            end if;
        end loop;
        for I in ValidArr'Range loop
            if ValidArr(I).Coordinates = CurrMove.To then
                return Valid_Move;
            end if;
        end loop;
        return Invalid_Move;
    end KingTest;


    function HasValidMove(Board : in Board_t;
                          From  : in Coordinates_t;
                          To    : in Coordinates_t) return Boolean is
    begin
        -- TODO
        return True;
    end HasValidMove;

    function FindPiece(Board           : in  Board_t;
                       CurrMove        : in  Move_t;
                       CurrPlayerColor : in  Color_t;
                       From            : out Coordinates_t) return MoveResult_t
    is
        package Integer_Vectors is new Ada.Containers.Vectors
            (Index_Type   => Positive,
             Element_Type => Coordinates_t);
        use Integer_Vectors;

        Pieces   : Vector;
        CurrCell : Cell_t;
    begin
        for File in a .. h loop
            for Rank in 1 .. 8 loop
                CurrCell := Board(File, Rank);

                if CurrCell.IsEmpty = False                          -- non empty cell
                  and CurrCell.Color = CurrPlayerColor               -- player color
                  and CurrCell.Piece = CurrMove.Piece                -- piece
                  and HasValidMove(Board, (File, Rank), CurrMove.To) -- legal move
                then
                    Put_Line("Found candidate piece: " & Image(Board(File, Rank)));
                    Pieces.Append((File, Rank));
                end if;
            end loop;
        end loop;

        case Pieces.Length is
            when 0 =>
                Put_Line("No valid piece found");
                return Invalid_Move;
            when 1 =>
                From := Pieces.First_Element;
                return Valid_Move;
            when others =>
                Put("Unresolved ambiguity between the pieces:");
                for Piece of Pieces loop
                    Put(" '" & Image(Piece) & "'");
                end loop;
                Put_Line("");
                return Ambiguous_Move;
        end case;
    end FindPiece;


    function Move(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t is
        FromCoords : Coordinates_t;
        BoardPiece : Cell_t;
        MoveResult : MoveResult_t;
    begin
        MoveResult := FindPiece(Board, CurrMove, CurrPlayerColor, FromCoords); -- find the piece on the board

        if MoveResult /= Valid_Move then -- error if the piece was not found
            return MoveResult;
        end if;

        BoardPiece := Board(FromCoords.File, FromCoords.Rank); -- get the piece at the given coords

        case BoardPiece.Piece is
            when King =>
                return KingTest(Board, CurrMove, CurrPlayerColor);

            when others =>
                null;
        end case;
        return Valid_Move;
    end Move;

    function Game_Ended(Board : in Board_t) return GameResult_t is
    begin
        -- TODO
        return Playing;
    end Game_Ended;

end Board;
