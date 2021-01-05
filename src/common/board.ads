package board is

    -- Board file
    type File_t is (A, B, C, D, E, F, G, H);
    -- Board rank
    subtype Rank_t is Positive range 1 .. 8;

    -- Coordinates on a board (ex: 'a5')
    type Coordinates_t(Undefined : Boolean := False) is
       record
           case Undefined is
           when True =>
               null;
           when False =>
               File : File_t;
               Rank : Rank_t;
           end case;
       end record;

    -- Pair of coordinates describing a move from x to y
    type Move_t is
        record
            From : Coordinates_t;
            To   : Coordinates_t;
        end record;


    -- The piece colors
    type Color_t is (White, Black);
    -- The pieces
    type Piece_t is (King, Queen, Rook, Bishop, Knight, Pawn);

    -- Record describing the pieces as they are present on a cell.
    -- A cell is either Empty or not empty, in which case it is a
    -- combination of the piece and its color.
    type BoardPiece_t(IsEmpty : Boolean := True) is
        record
            case IsEmpty is
                when True =>
                    null;
                when False =>
                    Piece : Piece_t;
                    Color : Color_t;
            end case;
        end record;

    type Board_t is array (File_t, Rank_t) of BoardPiece_t;


    -- Enumeration of the possible results after a move is given by
    -- a player. Used in the function Move().
    -- TODO add more errors
    type MoveResult_t is (Success, Invalid_Move);

    -- The differents outcomes of a game
    type GameResult_t is (Playing, Check, Checkmate, Resignation);


    Empty   : constant BoardPiece_t := (IsEmpty => True);

    BKing   : constant BoardPiece_t := (False, King, Black);
    BQueen  : constant BoardPiece_t := (False, Queen, Black);
    BRook   : constant BoardPiece_t := (False, Rook, Black);
    BBishop : constant BoardPiece_t := (False, Bishop, Black);
    BKnight : constant BoardPiece_t := (False, Knight, Black);
    BPawn   : constant BoardPiece_t := (False, Pawn, Black);

    WKing   : constant BoardPiece_t := (False, King, White);
    WQueen  : constant BoardPiece_t := (False, Queen, White);
    WRook   : constant BoardPiece_t := (False, Rook, White);
    WBishop : constant BoardPiece_t := (False, Bishop, White);
    WKnight : constant BoardPiece_t := (False, Knight, White);
    WPawn   : constant BoardPiece_t := (False, Pawn, White);


    -- Default board, white being on the ranks 1-2 and black on the
    -- ranks 7-8. BQueen is at the coordinates (D, 8). It is better
    -- to copy it at each game, that way we are able to restart the
    -- game without restarting the server.
    StartBoard : constant Board_t :=
    (--    a       b        c       d       e       f        g       h
        (WRook, WKnight, WBishop, WQueen, WKing, WBishop, WKnight, WRook), -- 1
        (WPawn, WPawn,   WPawn,   WPawn,  WPawn, WPawn,   WPawn,   WPawn), -- 2
        (Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty), -- 3
        (Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty), -- 4
        (Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty), -- 5
        (Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty), -- 6
        (BPawn, BPawn,   BPawn,   BPawn,  BPawn, BPawn,   BPawn,   BPawn), -- 7
        (BRook, BKnight, BBishop, BQueen, BKing, BBishop, BKnight, BRook)  -- 8
    );

    -- Move the piece on CurrMove.From to the position CurrMove.To
    -- on the given chess board Board.
    function Move(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t;

    -- Checks if the game has ended, either not, either someone wins
    -- or it is a tie.
    function Game_Ended(Board : in Board_t) return GameResult_t;

end board;
