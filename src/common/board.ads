package Board is

    -- Board file
    type File_t is (a, b, c, d, e, f, g, h);
    -- Board rank
    subtype Rank_t is Positive range 1 .. 8;

    -- Coordinates on a board (ex: 'a5')
    type Coordinates_t is
        record
            File : File_t;
            Rank : Rank_t;
        end record;

    type Has_Coordinates_t is (Has_None, Has_File, Has_Rank, Has_Both);
    type Opt_Coordinates_t(Has : Has_Coordinates_t := Has_Both) is
        record
            case Has is
                when Has_None =>
                    null;
                when Has_File =>
                    File : File_t;
                when Has_Rank =>
                    Rank : Rank_t;
                when Has_Both =>
                    Coordinates : Coordinates_t;
            end case;
        end record;


    -- The piece colors
    type Color_t is (White, Black);
    -- The pieces
    type Piece_t is (King, Queen, Rook, Bishop, Knight, Pawn);

    -- Record describing the pieces as they are present on a cell.
    -- A cell is either Empty or not empty, in which case it is a
    -- combination of the piece and its color.
    type Cell_t(IsEmpty : Boolean := True) is
        record
            case IsEmpty is
                when True =>
                    null;
                when False =>
                    Piece : Piece_t;
                    Color : Color_t;
            end case;
        end record;

    type Board_t is array (File_t, Rank_t) of Cell_t;


    -- A move is described by a destination and a prefix.
    type Move_t is
        record
            Piece   : Piece_t;
            Capture : Boolean;
            From    : Opt_Coordinates_t;
            To      : Coordinates_t;
        end record;


    -- Enumeration of the possible results after a move is given by
    -- a player. Used in the function Move().
    -- TODO add more errors
    type MoveResult_t is (Valid_Move, Invalid_Move);

    -- The differents outcomes of a game
    type GameResult_t is (Playing, Check, Checkmate, Resignation);


    Empty   : constant Cell_t := (IsEmpty => True);

    BKing   : constant Cell_t := (IsEmpty => False, Piece => King,   Color => Black);
    BQueen  : constant Cell_t := (IsEmpty => False, Piece => Queen,  Color => Black);
    BRook   : constant Cell_t := (IsEmpty => False, Piece => Rook,   Color => Black);
    BBishop : constant Cell_t := (IsEmpty => False, Piece => Bishop, Color => Black);
    BKnight : constant Cell_t := (IsEmpty => False, Piece => Knight, Color => Black);
    BPawn   : constant Cell_t := (IsEmpty => False, Piece => Pawn,   Color => Black);

    WKing   : constant Cell_t := (IsEmpty => False, Piece => King,   Color => White);
    WQueen  : constant Cell_t := (IsEmpty => False, Piece => Queen,  Color => White);
    WRook   : constant Cell_t := (IsEmpty => False, Piece => Rook,   Color => White);
    WBishop : constant Cell_t := (IsEmpty => False, Piece => Bishop, Color => White);
    WKnight : constant Cell_t := (IsEmpty => False, Piece => Knight, Color => White);
    WPawn   : constant Cell_t := (IsEmpty => False, Piece => Pawn,   Color => White);


    -- Default board, white being on the ranks 1-2 and black on the
    -- ranks 7-8. BQueen is at the coordinates (d, 8). It is better
    -- to mark it as constant and copy it at each game, that way we
    -- are able to restart the game without restarting the server.
    StartBoard : constant Board_t :=
    (--    1        2      3      4      5      6      7      8
        (WRook,   WPawn, Empty, Empty, Empty, Empty, BPawn, BRook  ), -- a
        (WKnight, WPawn, Empty, Empty, Empty, Empty, BPawn, BKnight), -- b
        (WBishop, WPawn, Empty, Empty, Empty, Empty, BPawn, BBishop), -- c
        (WQueen,  WPawn, Empty, Empty, Empty, Empty, BPawn, BQueen ), -- d
        (WKing,   WPawn, Empty, Empty, Empty, Empty, BPawn, BKing  ), -- e
        (WBishop, WPawn, Empty, Empty, Empty, Empty, BPawn, BBishop), -- f
        (WKnight, WPawn, Empty, Empty, Empty, Empty, BPawn, BKnight), -- g
        (WRook,   WPawn, Empty, Empty, Empty, Empty, BPawn, BRook  )  -- h
    );

    function Image(Coordinates : Coordinates_t) return String;
    function Image(Coordinates : Opt_Coordinates_t) return String;
    -- Converts the coordinates pair to an algebraic notation.
    function Image(Move : Move_t) return String;

    -- Converts an algebraic string to a coordinates pair.
    function Value(Move_Str : String) return Move_t;


    -- Move the piece on CurrMove.From to the position CurrMove.To
    -- on the given chess board Board.
    function Move(Board : in out Board_t; CurrMove : in Move_t; CurrPlayerColor : in Color_t) return MoveResult_t;

    -- Checks if the game has ended, either not, either someone wins
    -- or it is a tie.
    function Game_Ended(Board : in Board_t) return GameResult_t;

end Board;
