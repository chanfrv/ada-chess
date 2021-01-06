with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Board.Parse; use Board.Parse;


package body Board is


    function IsKingCheck(Board : in Board_t;
                         Pos   : in Coordinates_t;
                         Color : in Color_t) return Boolean is
    begin
        -- TODO
        -- 1) build a vector with all the opponent pieces
        -- 2) keep all the pieces with a valid move to the king
        -- 3) return list not empty
        return False;
    end IsKingCheck;


    function IsValidMove_King(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
        King_Color : Color_t := Board(From.File, From.Rank).Color;

        Min_File : File_t := (if From.File > a then File_t'Pred(From.File) else From.File);
        Max_File : File_t := (if From.File < h then File_t'Succ(From.File) else From.File);
        Min_Rank : Rank_t := (if From.Rank > 1 then From.Rank - 1 else From.Rank);
        Max_Rank : Rank_t := (if From.Rank < 8 then From.Rank + 1 else From.Rank);
    begin
        return Min_File <= To.File and To.File <= Max_File -- file inbound
          and Min_Rank <= To.Rank and To.Rank <= Max_Rank -- rank inbound
          and (From.File /= To.File or From.Rank /= To.Rank) -- cannot stay on the same cell
          and (Board(To.File, To.Rank).IsEmpty = True or Board(To.File, To.Rank).Color /= King_Color) -- either empty cell or opponent
          and not IsKingCheck(Board, To, King_Color); -- cannot move on a threatened cell
    end IsValidMove_King;


    function IsValidMove(Board : in Board_t;
                         From  : in Coordinates_t;
                         To    : in Coordinates_t) return Boolean
    is
        Cell  : constant Cell_t  := Board(From.File, From.Rank);
    begin
        -- TODO
        case Cell.Piece is
            when King =>
                return IsValidMove_King(Board, From, To);
            when others =>
                Put_Line("Move for piece not implemented");
                return False;
        end case;
    end IsValidMove;


    function FindPiece(Board           : in  Board_t;
                       CurrMove        : in  Move_t;
                       CurrPlayerColor : in  Color_t;
                       From            : out Coordinates_t) return MoveResult_t
    is
        package Coordinate_Vector is new Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => Coordinates_t);
        use Coordinate_Vector;

        Pieces   : Vector;
        CurrCord : Coordinates_t;
        CurrCell : Cell_t;

        From_Has_File : Boolean := CurrMove.From.Has = Has_File or CurrMove.From.Has = Has_Both;
        From_Has_Rank : Boolean := CurrMove.From.Has = Has_Rank or CurrMove.From.Has = Has_Both;

        Min_File : File_t := (if From_Has_File then CurrMove.From.File else a);
        Max_File : File_t := (if From_Has_File then CurrMove.From.File else h);
        Min_Rank : Rank_t := (if From_Has_Rank then CurrMove.From.Rank else 1);
        Max_Rank : Rank_t := (if From_Has_Rank then CurrMove.From.Rank else 8);
    begin
        for File in Min_File .. Max_File loop
            for Rank in Min_Rank .. Max_Rank loop
                CurrCord := (File, Rank);
                CurrCell := Board(File, Rank);

                case CurrCell.IsEmpty is
                    when True =>
                        null;
                    when False =>                                           -- non empty cell
                        if CurrCell.Color = CurrPlayerColor                 -- player color
                          and CurrCell.Piece = CurrMove.Piece               -- piece
                          and IsValidMove(Board, (File, Rank), CurrMove.To) -- legal move
                        then
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
        -- 1) remove the captured pieces (! en passant)
        -- 2) execute the move

        return Valid_Move;
    end Move;

    function Game_Ended(Board : in Board_t) return GameResult_t is
    begin
        -- TODO
        return Playing;
    end Game_Ended;

end Board;
