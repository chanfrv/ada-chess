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
        Cell : Cell_t;
    begin
        for File in a .. h loop
            for Rank in 1 .. 8 loop
                Cell := Board(File, Rank);

                case Cell.IsEmpty is
                    when True =>
                        null;
                    when False =>
                        if IsValidMove(Board, (File, Rank), Pos) then
                            return True;
                        end if;
                end case;
            end loop;
        end loop;

        return False;
    end IsKingCheck;


    function IsValidMove_King(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
        King_Color : Color_t := Board(From.File, From.Rank).Color;
        Cell : Cell_t := Board(To.File, To.Rank);

        -- Gets the rectangle of dimension file and rank where the king is
        -- allowed to move without going out of bounds.
        Min_File : File_t := (if From.File > a then File_t'Pred(From.File) else From.File);
        Max_File : File_t := (if From.File < h then File_t'Succ(From.File) else From.File);
        Min_Rank : Rank_t := (if From.Rank > 1 then From.Rank - 1 else From.Rank);
        Max_Rank : Rank_t := (if From.Rank < 8 then From.Rank + 1 else From.Rank);
    begin
        -- The king move is valid if:
        -- + the file is inbound
        -- + the rank is inbound
        -- + the cell is different from the origin
        -- + either the cell is empty or it belongs to the opponent
        -- + the destination cell is not threatened
        return Min_File <= To.File and To.File <= Max_File
          and Min_Rank <= To.Rank and To.Rank <= Max_Rank
          and (From.File /= To.File or From.Rank /= To.Rank)
          and (case Cell.IsEmpty is when True => True, when False => Board(To.File, To.Rank).Color /= King_Color)
          and not IsKingCheck(Board, To, King_Color);
    end IsValidMove_King;


    function IsValidMove_Pawn(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
        Pawn_Color : Color_t := Board(From.File, From.Rank).Color;
    begin
        -- TODO
        return True;
    end IsValidMove_Pawn;


    function IsValidMove(Board : in Board_t;
                         From  : in Coordinates_t;
                         To    : in Coordinates_t) return Boolean
    is
        Cell : constant Cell_t  := Board(From.File, From.Rank);
    begin
        -- TODO
        -- Dispatch to the piece specific function
        case Cell.Piece is
            when King =>
                return IsValidMove_King(Board, From, To);
            when Pawn =>
                return IsValidMove_Pawn(Board, From, To);
            when others =>
                Put_Line("Move for piece " & Cell.Piece'Image & " not implemented");
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

        -- The move contains the origin file or rank information
        From_Has_File : Boolean := CurrMove.From.Has = Has_File or CurrMove.From.Has = Has_Both;
        From_Has_Rank : Boolean := CurrMove.From.Has = Has_Rank or CurrMove.From.Has = Has_Both;

        -- Decide of the bounds where to search the piece. If a player inputs
        -- 'e4' neither the origin file nor the ranks are given so the piece
        -- will be searched on both (a .. h) and (1 .. 8). If a player inputs
        -- 'exd4' the file 'e' is given so the search will be restricted to the
        -- pieces from the file range (e .. e).
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
                        -- The cell is empty
                        null;
                    when False =>
                        -- the cell is not empty, the move must validate:
                        -- + the piece belongs to the player
                        -- + the given piece type (ex: 'Be4' matches only bishops)
                        -- + the move is legal
                        if CurrCell.Color = CurrPlayerColor
                          and CurrCell.Piece = CurrMove.Piece
                          and IsValidMove(Board, (File, Rank), CurrMove.To)
                        then
                            -- The move is possible, we add the piece position
                            -- to the candidates
                            Put_Line("Found candidate '" & Image(CurrCord) & "'");
                            Pieces.Append((File, Rank));
                        end if;
                end case;
            end loop;
        end loop;

        case Pieces.Length is
            when 0 => -- no piece found: invalid move
                Put_Line("No valid piece found");
                return Invalid_Move;
            when 1 => -- one piece found
                From := Pieces.First_Element;
                return Valid_Move;
            when others => -- more than one piece found: ambiguity
                Put("Unresolved ambiguity between the pieces:");
                for Piece of Pieces loop
                    Put(" '" & Image(Piece) & "'");
                end loop;
                Put_Line("");
                return Ambiguous_Move;
        end case;
    end FindPiece;


    function Move(Board           : in out Board_t;
                  CurrMove        : in Move_t;
                  CurrPlayerColor : in Color_t) return MoveResult_t
    is
        From : Coordinates_t;
        BoardPiece : Cell_t;
        MoveResult : MoveResult_t;
    begin
        -- find the piece on the board
        MoveResult := FindPiece(Board, CurrMove, CurrPlayerColor, From);

        if MoveResult = Valid_Move then
            -- if the move is valid, move the piece
            Board(CurrMove.To.File, CurrMove.To.Rank) := Board(From.File, From.Rank);
            Board(From.File, From.Rank) := (IsEmpty => True);
            -- TODO en passant
        end if;

        return MoveResult;
    end Move;

    function Game_Ended(Board : in Board_t) return GameResult_t is
    begin
        -- TODO
        return Playing;
    end Game_Ended;

end Board;
