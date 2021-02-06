with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Exceptions; use Ada.Exceptions;

with Board.Strings; use Board.Strings;
with Board.Castling; use Board.Castling;
with Board.EnPassant; use Board.EnPassant;
with Board.Strings.Pretty; use Board.Strings.Pretty;
with Logs;


package body Board is


    function IsCellAccessible(Board       : in Board_t;
                              To          : in Coordinates_t;
                              PlayerColor : in Color_t;
                              Capture     : in Boolean) return Boolean
    is
        Cell : Opt_Cell_t := Board(To.File, To.Rank);
    begin
        case Capture is
            when True =>
                return (not Cell.IsEmpty and then Cell.Value.Color /= PlayerColor)
                  or (Cell.IsEmpty and (EnPassant_Coords.IsEnPassant and then To = EnPassant_Coords.To));
            when False =>
                return Cell.IsEmpty;
        end case;
    end IsCellAccessible;


    function IsKingCheckAt(Board : in Board_t;
                           From  : in Coordinates_t;
                           To    : in Coordinates_t) return Boolean
    is
        NewBoard  : Board_t    := Board;
        King      : Opt_Cell_t := Board(From.File, From.Rank);
    begin
        Logs.Debug("Checking if the king would be checked on " & Image(To));

        -- Move the King
        NewBoard(To.File, To.Rank) := King;
        NewBoard(From.File, From.Rank) := Empty;

        -- See if the king would be check at this position
        return IsKingCheck(NewBoard, To);
    end IsKingCheckAt;


    function IsKingCheck(Board : in Board_t;
                         To    : in Coordinates_t) return Boolean
    is
        King : Opt_Cell_t := Board(To.File, To.Rank);
        Cell : Opt_Cell_t;
        From : Coordinates_t;
    begin
        Logs.Debug("Determining if " & Image(King) & " on " & Image(To) & " is check...");
        -- Find an opponent piece threatening it
        for File in a .. h loop
            for Rank in 1 .. 8 loop
                Cell := Board(File, Rank);
                From := (file, Rank);
                Logs.Inc_Indent;
                if IsValidMove(Board, From, To, True, Promotion_Empty) then
                    Logs.Debug(Image(King) & " is check by " & Image(Cell) & " (" & Image(From) & ")");
                    Logs.Dec_Indent;
                    return True;
                end if;
                Logs.Dec_Indent;
            end loop;
        end loop;

        Logs.Debug(Image(King) & " is not check");
        return False;
    end IsKingCheck;


    function IsKingCheckmate(Board : in Board_t;
                             To    : in Coordinates_t) return Boolean
    is
        King     : Opt_Cell_t := Board(To.File, To.Rank);
        Opponent : Color_t    := (if King.Value.Color = White then Black else White);

        From_Coords : Coordinates_t;
        From_Cell   : Opt_Cell_t;
        To_Coords   : Coordinates_t;
    begin
        Logs.Debug("Determining if " & Image(King) & " is checkmate...");
        -- Iterate on all the allied pieces
        for File_From in a .. h loop
            for Rank_From in 1 .. 8 loop

                From_Cell := Board(File_From, Rank_From);
                From_Coords := (File_From, Rank_From);

                if not From_Cell.IsEmpty and then From_Cell.Value.Color = King.Value.Color then
                    -- Try to move the piece everywhere and check if the king is
                    -- not checked
                    Logs.Debug("Trying to move piece " & Image(From_Cell));
                    Logs.Inc_Indent;

                    for File_To in a .. h loop
                        for Rank_To in 1 ..8 loop

                            To_Coords := (File_To, Rank_To);

                            if (IsValidMove(Board, From_Coords, To_Coords, False, Promotion_Empty)
                                 or IsValidMove(Board, From_Coords, To_Coords, True, Promotion_Empty))
                            then
                                Logs.Debug(Image(King) & " is not checkmate");
                                Logs.Dec_Indent;
                                return False;
                            end if;

                        end loop;
                    end loop;

                    Logs.Dec_Indent;

                end if;

            end loop;
        end loop;
        Logs.Debug(Image(King) & " is checkmate");
        return True;
    end IsKingCheckmate;


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
        return Min_File <= To.File and To.File <= Max_File
          and Min_Rank <= To.Rank and To.Rank <= Max_Rank;
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

        Cell : Opt_Cell_t;
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

        Cell    : Opt_Cell_t;
        File    : File_t;
        Rank    : Rank_t;
        -- Ascending or descending diagnoal iteratior
        Rank_It : Integer;
    begin
        -- Is a diagonal
        if abs (File_t'Pos(From.File) - File_t'Pos(To.File)) = abs (From.Rank - To.Rank) then
            File    := File_t'Succ(Min_File);
            Rank_It := (if Min_File_Rank < Max_File_Rank then 1 else -1);
            Rank    := Min_File_Rank + Rank_It;

            -- traverse the diagonal
            --while File /= File_t'Pred(Max_File) and Rank /= Max_File_Rank - Rank_It loop
            while File /= Max_File and Rank /= Max_File_Rank loop
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


    function IsValidMove_Pawn(Board     : in Board_t;
                              From      : in Coordinates_t;
                              To        : in Coordinates_t;
                              Promotion : in Opt_Promotion_t) return Boolean
    is
        Cell_From  : constant Opt_Cell_t := Board(From.File, From.Rank);
        Cell_To    : constant Opt_Cell_t := Board(To.File, To.Rank);
        Pawn_Color : Color_t := Cell_From.Value.Color;

        Forward_Valid   : Boolean;
        Diagonal_Valid  : Boolean;
        Promotion_Valid : Boolean;

        Moving_Forward  : Boolean := (if Pawn_Color = White then
                                         From.Rank < To.Rank else From.Rank > To.Rank);
        Pawn_Rank       : Rank_t  := (if Pawn_Color = White then 2 else 7);
        Next_Rank       : Rank_t  := (if Pawn_Color = White then From.Rank + 1 else From.Rank - 1);
        Opp_Home_Rank   : Rank_t  := (if Pawn_Color = White then 8 else 1);
    begin

        -- Can only move forward
        if not Moving_Forward then
            return False;
        end if;

        -- On a forward move the cell is empty
        Forward_Valid := Cell_To.IsEmpty
          -- and we stay on the same file
          and then (From.File = To.File
                     -- and we move either for one rank or 2 if it is its first move
                     and (case abs (To.Rank - From.Rank) is
                               when 1 => True,
                               when 2 => Board(From.File, Next_Rank).IsEmpty,
                               when others => False));

        -- ...Or we capture on a diagnoal
        Diagonal_Valid :=
          -- The cell is not empty or empty and marked as en passant
          (not Cell_To.IsEmpty or (EnPassant_Coords.IsEnPassant and then To = EnPassant_Coords.To))
          and then
            -- Or we capture, one cell forward, one cell on the left (if it is inbounds)
            ((if From.File > a then
                 (File_t'Pos(From.File) - 1 = File_t'Pos(To.File) and Next_Rank = To.Rank)
               else False)
            -- Or we capture, one cell forward, one cell on the right (if it is inbounds)
              or (if From.File < h then
                     (File_t'Pos(From.File) + 1 = File_t'Pos(To.File) and Next_Rank = To.Rank)
                   else False));

        Promotion_Valid := (if To.Rank = Opp_Home_Rank then not Promotion.IsEmpty else True);

        return (Forward_Valid or Diagonal_Valid) and Promotion_Valid;
    end IsValidMove_Pawn;


    function IsValidMove(Board     : in Board_t;
                         From      : in Coordinates_t;
                         To        : in Coordinates_t;
                         Capture   : in Boolean;
                         Promotion : in Opt_Promotion_t) return Boolean
    is
        function GetKing(Board : in Board_t; Color : in Color_t) return Coordinates_t
        is
            King : Opt_Cell_t := (if Color = White then WKing else BKing);
        begin
            for File in a .. h loop
                for Rank in 1 .. 8 loop
                    if Board(File, Rank) = King then
                        return (File, Rank);
                    end if;
                end loop;
            end loop;
            raise Program_Error with "No King !";
        end GetKing;

        Cell_From : constant Opt_Cell_t := Board(From.File, From.Rank);
        Cell_To   : constant Opt_Cell_t := Board(To.File, To.Rank);

        Tmp_Board : Board_t;
        Tmp_King  : Coordinates_t;
        Opp_King  : Coordinates_t;
    begin
        if Cell_From.IsEmpty then
            return False;
        end if;

        Logs.Debug("Checking piece " & Image(Board(From.File, From.Rank))
                   & " going from '" & Image(From) & "' to '" & Image(To) & "'");

        -- Rules common to every piece:
        -- + the cell is different from the origin
        -- + either the cell is empty or it belongs to the opponent
        if (From.File = To.File and From.Rank = To.Rank)
          or not IsCellAccessible(Board, To, Cell_From.Value.Color, Capture)
        then
            Logs.Debug("Cell not accessible because of common rules");
            return False;
        end if;

        -- Piece specific rules
        if not (case Cell_From.Value.Piece is
                     when King   => IsValidMove_King  (Board, From, To),
                     when Queen  => IsValidMove_Queen (Board, From, To),
                     when Rook   => IsValidMove_Rook  (Board, From, To),
                     when Bishop => IsValidMove_Bishop(Board, From, To),
                     when Knight => IsValidMove_Knight(Board, From, To),
                     when Pawn   => IsValidMove_Pawn  (Board, From, To, Promotion))
        then
            Logs.Debug("Cell not accessible because of piece specific rules");
            return False;
        end if;

        Opp_King := GetKing(Board, (if Cell_From.Value.Color = White then Black else White));

        if To = Opp_King then
            Logs.Debug("Opponent king already check");
            return True;
        end if;

        -- Is King check with this move ?
        Tmp_Board := Board;
        Logs.Inc_Indent;

        Tmp_Board(To.File, To.Rank) := Tmp_Board(From.File, From.Rank);
        Tmp_Board(From.File, From.Rank) := Empty;

        Tmp_King := GetKing(Tmp_Board, Cell_From.Value.Color);

        if IsKingCheck(Tmp_Board, Tmp_King) then
            Logs.Debug("This move would make the allied King check");
            Logs.Dec_Indent;
            return False;
        else
            Logs.Dec_Indent;
            return True;
        end if;

    end IsValidMove;


    function FindPiece(Board           : in  Board_t;
                       CurrMove        : in  Move_t;
                       CurrPlayerColor : in  Color_t;
                       From            : out Coordinates_t) return MoveResult_t
    is
        CurrCord : Coordinates_t;
        CurrCell : Opt_Cell_t;

        Found : Boolean := False;

        -- The move contains the origin file or rank information
        From_Has_File : Boolean := not CurrMove.From_File.IsEmpty;
        From_Has_Rank : Boolean := not CurrMove.From_Rank.IsEmpty;

        -- Decide of the bounds where to search the piece. If a player inputs
        -- 'e4' neither the origin file nor the ranks are given so the piece
        -- will be searched on both (a .. h) and (1 .. 8). If a player inputs
        -- 'exd4' the file 'e' is given so the search will be restricted to the
        -- pieces from the file range (e .. e).
        Min_File : File_t := (if From_Has_File then CurrMove.From_File.Value else a);
        Max_File : File_t := (if From_Has_File then CurrMove.From_File.Value else h);
        Min_Rank : Rank_t := (if From_Has_Rank then CurrMove.From_Rank.Value else 1);
        Max_Rank : Rank_t := (if From_Has_Rank then CurrMove.From_Rank.Value else 8);
    begin
        Logs.Debug("Looking for a piece that can move to " & Image(CurrMove.To));

        for File in Min_File .. Max_File loop
            for Rank in Min_Rank .. Max_Rank loop
                CurrCord := (File, Rank);
                CurrCell := Board(File, Rank);

                -- the cell is not empty, the move must validate:
                -- + the piece belongs to the player
                -- + the given piece type (ex: 'Be4' matches only bishops)
                -- + the move is legal
                if not CurrCell.IsEmpty
                  and then (CurrCell.Value.Color = CurrPlayerColor and CurrCell.Value.Piece = CurrMove.Piece)
                  and then IsValidMove(Board, (File, Rank), CurrMove.To, CurrMove.Capture, CurrMove.Promotion)
                then
                    -- The move is possible
                    Logs.Debug("Found candidate '" & Image(CurrCord) & "'");
                    if Found = True then
                        Logs.Debug("Unresolved ambiguity with " & Image(CurrCord));
                        return Ambiguous_Move;
                    else
                        From := (File, Rank);
                        Found := True;
                    end if;
                end if;
            end loop;
        end loop;

        case Found is
            when True  => -- one piece found
                return Valid_Move;
            when False => -- no piece found: invalid move
                Logs.Debug("No valid piece found");
                return Invalid_Move;
        end case;
    end FindPiece;


    function Move(Board           : in out Board_t;
                  CurrMove        : in Move_t;
                  CurrPlayerColor : in Color_t) return MoveResult_t
    is
        From       : Coordinates_t;
        MoveResult : MoveResult_t;
        Cell_To    : Cell_t;
        Promoted   : Cell_t;
    begin
        case CurrMove.Castling is
            when Kingside =>
                Logs.Debug("Trying kingside castling");
                return Castling_Kingside(Board, CurrPlayerColor);
            when Queenside =>
                Logs.Debug("Trying queenside castling");
                return Castling_Queenside(Board, CurrPlayerColor);
            -- Normal move
            when None =>
                Logs.Debug("Normal move, looking for a valid piece");
                -- find the piece on the board
                MoveResult := FindPiece(Board, CurrMove, CurrPlayerColor, From);

                if MoveResult = Valid_Move then
                    -- if the piece is a king or rook, unregister the piece for castling
                    Castling_Unregister(Board, From);

                    -- if the move is valid, move the piece
                    Logs.Debug("Found piece " & Image(Board(From.File, From.Rank))
                               & " on '" & Image(From) & "'");
                    Board(CurrMove.To.File, CurrMove.To.Rank) := Board(From.File, From.Rank);
                    Board(From.File, From.Rank) := Empty;

                    -- En passant handling
                    EnPassant_Coords := EnPassant_Handler(Board,
                                                          CurrMove.Piece,
                                                          CurrPlayerColor,
                                                          CurrMove.Capture,
                                                          From,
                                                          CurrMove.To);

                    -- Promotion
                    Cell_To := Board(CurrMove.To.File, CurrMove.To.Rank).Value;
                    if not CurrMove.Promotion.IsEmpty
                         and then (Cell_To.Piece = Pawn
                             and ((Cell_To.Color = White and CurrMove.To.Rank = 8)
                               or (Cell_To.Color = Black and CurrMove.To.Rank = 1)))
                    then
                        Promoted := (Piece => CurrMove.Promotion.Value, Color => Cell_To.Color);
                        Board(CurrMove.To.File, CurrMove.To.Rank) := (IsEmpty => False, Value => Promoted);
                    end if;
                end if;
        end case;

        return MoveResult;
    end Move;


    function Game_Ended(Board           : in Board_t;
                        CurrPlayerColor : in Color_t) return GameResult_t
    is
        Opponent  : Color_t    := (if CurrPlayerColor = White then Black else White);
        King      : Opt_Cell_t := (if Opponent        = White then WKing else BKing);

        King_Coords : Coordinates_t;
    begin
        Logs.Debug("Processing game status");
        for File in a .. h loop
            for Rank in 1 .. 8 loop

                if Board(File, Rank) = King then
                    -- Is the king check ?
                    King_Coords := (File, Rank);
                    if IsKingCheck(Board, King_Coords) then
                        Logs.Debug("Check, is it checkmate?");
                        -- Checkmate ?
                        if IsKingCheckmate(Board, King_Coords) then
                            Logs.Debug("Checkmate");
                            return Checkmate;
                        else
                            Logs.Debug("Check");
                            return Check;
                        end if;
                    end if;
                    -- the king is not check
                    exit;
                end if;
            end loop;
        end loop;

        return Playing;
    end Game_Ended;


end Board;
