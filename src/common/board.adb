with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Board.Parse; use Board.Parse;


package body Board is


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
                          To    : in Coordinates_t) return Boolean
    is
        Cell  : constant Cell_t  := Board(From.File, From.Rank);
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
        CurrCord : Coordinates_t;
        CurrCell : Cell_t;
    begin
        for File in a .. h loop
            for Rank in 1 .. 8 loop
                CurrCord := (File, Rank);
                CurrCell := Board(File, Rank);

                case CurrCell.IsEmpty is
                    when True =>
                        null;
                    when False =>                                            -- non empty cell
                        if CurrCell.Color = CurrPlayerColor                  -- player color
                          and CurrCell.Piece = CurrMove.Piece                -- piece
                          and HasValidMove(Board, (File, Rank), CurrMove.To) -- legal move
                        then
                            Put_Line("Found candidate piece: " & Image(Board(File, Rank)) & " (" & Image(CurrCord) & ")");
                            Pieces.Append((File, Rank));
                        end if;
                end case;
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

        -- TODO now that the move is valid
        -- 1. remove the captured pieces (! en passant)
        -- 2. execute the move

        case CurrMove.Piece is
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
