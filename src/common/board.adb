package body board is

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
        TmpPiece : BoardPiece_t;
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
                return Success
            end if;
        end loop;
        return Invalid_Move;
    end KingTest;

    function Move(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t is
        BoardPiece: BoardPiece_t;
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
        return Success;
    end Move;

    function Game_Ended(Board : in Board_t) return GameResult_t is
    begin
        -- TODO
        return Playing;
    end Game_Ended;

end board;
