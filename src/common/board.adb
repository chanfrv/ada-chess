with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Board.Parse; use Board.Parse;


package body Board is


    function IsKingCheckAt(Board     : in Board_t;
                           Origin    : in Coordinates_t;
                           Objective : in Coordinates_t) return Boolean
    is
        NewBoard  : Board_t := Board;
        King      : Cell_t := Board(Origin.File, Origin.Rank);
    begin
        Put_Line("Checking if the king would be checked on " & Image(Objective));

        -- Move the King
        NewBoard(Objective.File, Objective.Rank) := King;
        NewBoard(Origin.File, Origin.Rank) := (IsEmpty => True);

        -- See if the king would be check at this position
        return IsKingCheck(NewBoard, Objective);
    end IsKingCheckAt;


    function IsKingCheck(Board : in Board_t;
                         Pos   : in Coordinates_t) return Boolean
    is
        Cell : Cell_t;
    begin
        -- Find an opponent piece threatening it
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


    function IsCellAccessible(Board       : in Board_t;
                              Cell        : in Cell_t;
                              PlayerColor : in Color_t) return Boolean
    is
    begin
        case Cell.IsEmpty is
            when True =>
                return True;
            when False =>
                return Cell.Color /= PlayerColor;
        end case;
    end IsCellAccessible;


    function IsValidMove_King(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
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
        -- + the destination cell is not threatened
        return Min_File <= To.File and then To.File <= Max_File
          and then Min_Rank <= To.Rank and then To.Rank <= Max_Rank
          and then not IsKingCheckAt(Board, From, To);
    end IsValidMove_King;


    function IsValidMove_Queen(Board : in Board_t;
                               From  : in Coordinates_t;
                               To    : in Coordinates_t) return Boolean
    is
    begin
        return IsValidMove_Rook(Board, From, To)
          or IsValidMove_Bishop(Board, From, To);
    end IsValidMove_Queen;


    function IsValidMove_Rook(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
        Min_File : File_t := (if From.File < To.File then From.File else To.File);
        Max_File : File_t := (if From.File > To.File then From.File else To.File);
        Min_Rank : Rank_t := (if From.Rank < To.Rank then From.Rank else To.Rank);
        Max_Rank : Rank_t := (if From.Rank > To.Rank then From.Rank else To.Rank);

        Cell : Cell_t;
    begin
        -- Same File
        if From.File = To.File then
            -- each cell between the pieces must be free
            for Rank in Min_Rank + 1 .. Max_Rank - 1 loop
                Cell := Board(From.File, Rank);
                if not Cell.IsEmpty then
                    return False;
                end if;
            end loop;
        -- Same Rank
        elsif From.Rank = To.Rank then
            -- each cell between the pieces must be free
            for File in File_t'Succ(Min_File) .. File_t'Pred(Max_File) loop
                Cell := Board(File, From.Rank);
                if not Cell.IsEmpty then
                    return False;
                end if;
            end loop;
        -- Neither same file nor rank, not a valid rook move
        else
            return False;
        end if;

        return True;
    end IsValidMove_Rook;


    function IsValidMove_Bishop(Board : in Board_t;
                                From  : in Coordinates_t;
                                To    : in Coordinates_t) return Boolean
    is
        Min_File : File_t := (if From.File < To.File then From.File else To.File);
        Max_File : File_t := (if From.File > To.File then From.File else To.File);
        Min_File_Rank : Rank_t := (if From.File < To.File then From.Rank else To.Rank);
        Max_File_Rank : Rank_t := (if From.File > To.File then From.Rank else To.Rank);

        Cell : Cell_t;

        File : File_t := File_t'Succ(Min_File);
        -- Ascending or descending diagnoal
        Rank_It : Integer := (if Min_File_Rank < Max_File_Rank then 1 else -1);
        Rank    : Rank_t := Min_File_Rank + Rank_It;
    begin
        -- Is a diagonal
        if abs (File_t'Pos(From.File) - File_t'Pos(To.File)) = abs (From.Rank - To.Rank) then
            -- traverse the diagonal
            while File /= File_t'Pred(Max_File) and Rank /= Max_File_Rank - Rank_It loop
                Cell := Board(File, Rank);
                -- Each cell on the diagonal must be free
                if not Cell.IsEmpty then
                    return False;
                end if;
                File := File_t'Succ(File);
                Rank := Rank + Rank_It;
            end loop;
        -- Not a diagonal
        else
            return False;
        end if;

        return True;
    end IsValidMove_Bishop;


    function IsValidMove_Knight(Board : in Board_t;
                                From  : in Coordinates_t;
                                To    : in Coordinates_t) return Boolean
    is
    begin
        -- Either moving 1 on the file, 2 on the rank; or 2 on the file, 1 on
        -- the rank.
        return (abs (File_t'Pos(From.File) - File_t'Pos(To.File)) = 1
                and abs (From.Rank - To.Rank) = 2)
            or (abs (File_t'Pos(From.File) - File_t'Pos(To.File)) = 2
                and abs (From.Rank - To.Rank) = 1);
    end IsValidMove_Knight;


    function IsValidMove_Pawn(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
    is
        Cell_From  : constant Cell_t := Board(From.File, From.Rank);
        Cell_To    : constant Cell_t := Board(To.File, To.Rank);
        Pawn_Color : Color_t := Cell_From.Color;

        Forward_Valid  : Boolean;
        Diagonal_Valid : Boolean;
    begin
        -- Moves reversed depending on the color
        case Pawn_Color is
            when White =>
                -- Can only move forward
                if From.Rank > To.Rank then
                    return False;
                end if;

                -- Either we me forward 1 cell (or 2 if we are on the rank 2)
                case Cell_To.IsEmpty is
                    when True =>
                        Forward_Valid := (From.File = To.File and abs (To.Rank - From.Rank) <= (if From.Rank = 2 then 2 else 1));
                    when False =>
                        Forward_Valid := False;
                end case;

                case Cell_To.IsEmpty is
                    when True =>
                        Diagonal_Valid := False;
                    when False =>
                        Diagonal_Valid :=
                          -- Or we capture, one cell forward, one cell on the left (if it is inbounds)
                          (if From.File > a then (File_t'Pos(From.File) - 1 = File_t'Pos(To.File) and From.Rank + 1 = To.Rank) else False)
                          -- Or we capture, one cell forward, one cell on the right (if it is inbounds)
                          or (if From.File < h then (File_t'Pos(From.File) + 1 = File_t'Pos(To.File) and From.Rank + 1 = To.Rank) else False);
                end case;

            when Black =>
                -- Can only move forward (but the other way)
                if From.Rank < To.Rank then
                    return False;
                end if;

                case Cell_To.IsEmpty is
                    when True =>
                        Forward_Valid := (From.File = To.File and abs (To.Rank - From.Rank) <= (if From.Rank = 7 then 2 else 1));
                    when False =>
                        Forward_Valid := False;
                end case;

                case Cell_To.IsEmpty is
                    when True =>
                        Diagonal_Valid := False;
                    when False =>
                        Diagonal_Valid :=
                          -- Or we capture, one cell forward, one cell on the left (if it is inbounds)
                          (if From.File > a then (File_t'Pos(From.File) - 1 = File_t'Pos(To.File) and From.Rank - 1 = To.Rank) else False)
                          -- Or we capture, one cell forward, one cell on the right (if it is inbounds)
                          or (if From.File < h then (File_t'Pos(From.File) + 1 = File_t'Pos(To.File) and From.Rank - 1 = To.Rank) else False);
                end case;
        end case;

        return Forward_Valid or Diagonal_Valid;
    end IsValidMove_Pawn;


    function IsValidMove(Board : in Board_t;
                         From  : in Coordinates_t;
                         To    : in Coordinates_t) return Boolean
    is
        Cell_From : constant Cell_t := Board(From.File, From.Rank);
        Cell_To   : constant Cell_t := Board(To.File, To.Rank);
    begin
        Put_Line("Checking piece " & Image(Board(From.File, From.Rank)) & " going from '" & Image(From) & "' to '" & Image(To) & "'");

        -- Rules common to every piece:
        -- + the cell is different from the origin
        -- + either the cell is empty or it belongs to the opponent
        if (From.File = To.File and From.Rank = To.Rank)
          or not IsCellAccessible(Board, Cell_To, Cell_From.Color)
        then
            return False;
        end if;

        -- Piece specific rules
        case Cell_From.Piece is
            when King =>
                return IsValidMove_King(Board, From, To);
            when Queen =>
                return IsValidMove_Queen(Board, From, To);
            when Rook =>
                return IsValidMove_Rook(Board, From, To);
            when Bishop =>
                return IsValidMove_Bishop(Board, From, To);
            when Knight =>
                return IsValidMove_Knight(Board, From, To);
            when Pawn =>
                return IsValidMove_Pawn(Board, From, To);
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
        Put_Line("Looking for a piece that can move to " & Image(CurrMove.To));

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
                          and then CurrCell.Piece = CurrMove.Piece
                          and then IsValidMove(Board, (File, Rank), CurrMove.To)
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
                New_Line;
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
        Cell_To : Cell_t;
        Promoted : Cell_t;
    begin
        Put_Line("Moving to " & Image(CurrMove.To));

        -- TODO castling

        -- find the piece on the board
        MoveResult := FindPiece(Board, CurrMove, CurrPlayerColor, From);

        if MoveResult = Valid_Move then
            -- if the move is valid, move the piece
            Board(CurrMove.To.File, CurrMove.To.Rank) := Board(From.File, From.Rank);
            Board(From.File, From.Rank) := (IsEmpty => True);

            Cell_To := Board(CurrMove.To.File, CurrMove.To.Rank);

            -- Promotion
            if Cell_To.Piece = Pawn and (
                    (Cell_To.Color = White and CurrMove.To.Rank = 8)
                 or (Cell_To.Color = Black and CurrMove.To.Rank = 1))
            then
                Promoted := (IsEmpty => False, Piece => CurrMove.Promotion, Color => Cell_To.Color);
                Board(CurrMove.To.File, CurrMove.To.Rank) := Promoted;
            end if;

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
