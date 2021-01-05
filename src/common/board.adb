with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;


package body Board is

    function Image(Move : Move_t) return String is
    begin
        -- TODO
        return "a1";
    end Image;

    procedure GetPieceDisambiguity(Move_Str : String; Move : in out Move_t) is
        Index : Positive;
        Curr : Character;
    begin
        Put_Line("Disambiguity substring: '" & Move_Str & "'");

        Index := Move_Str'Last;
        Curr := Move_Str(Index);
        Move.Piece := Pawn;

        case Curr is
            when '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8' =>
                Move.From.Rank := Rank_t'Value("" & Curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Move_Str'Last - 1);
                end if;

            when others =>
                null;
        end case;

        case Curr is
            when 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h' =>
                Move.From.File := File_t'Value("" & Curr);

                if Move_Str'Length > 1 then
                    Index := Index - 1;
                    Curr := Move_Str(Move_Str'Last - 1);
                end if;

            when others =>
                null;
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


    function GetCoordinates(File : Integer; Rank : Integer) return Coordinates_t is
    begin
        if File > File_t'Pos(File_t'Last) or File < File_t'Pos(File_t'First) then
            return (Undefined => True);
        end if;
        if Rank > Rank_t'Pos(Rank_t'Last) or Rank < Rank_t'Pos(Rank_t'First) then
            return (Undefined => True);
        end if;
        return (False, File_t'Val(File), Rank_t'Val(Rank));
    end GetCoordinates;


    function KingTest(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t is
        FileInt : Integer := File_t'Pos(CurrMove.From.File);
        RankInt : Integer := Rank_t'Pos(CurrMove.From.Rank);
        type Valid_Coord_Array is array (Integer) of Coordinates_t;
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
        if ValidArr(1) /= (Undefined => True) then
            return Invalid_Move;
        end if;
        for I in ValidArr'Range loop
            if ValidArr(I) /= (Undefined => True) then
                TmpPiece := Board(ValidArr(I).File, ValidArr(I).Rank);
                if TmpPiece.Color = CurrPlayerColor then
                    ValidArr(I) := (Undefined => True);
                end if;
            end if;
        end loop;
        for I in ValidArr'Range loop
            if ValidArr(I) = CurrMove.To then
                return Valid_Move;
            end if;
        end loop;
        return Invalid_Move;
    end KingTest;

    function Move(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t is
        BoardPiece: Cell_t;
    begin
        BoardPiece := Board(CurrMove.From.File, CurrMove.From.Rank);

        if BoardPiece.IsEmpty or BoardPiece.Color /= CurrPlayerColor then
            return Invalid_Move;
        end if;

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
