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

    -- Algebraic coordinates provides either no, only the file, only
    -- the rank or both coordinates of the moving piece to solve
    -- ambiguities between multiple pieces allowed to move to the
    -- destination cell.
    type Has_Coordinates_t is (Has_None, Has_File, Has_Rank, Has_Both);
    type Disambiguating_Coordinates_t(Has : Has_Coordinates_t := Has_Both) is
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
    type Cell_t(IsEmpty : Boolean := False) is
        record
            case IsEmpty is
                when True =>
                    null;
                when False =>
                    Piece : Piece_t;
                    Color : Color_t;
            end case;
        end record;


    -- Board type, a 2-dimensional array of cells
    type Board_t is array (File_t, Rank_t) of Cell_t;


    -- Castling state
    type Castling_t is (None, Kingside, Queenside);


    -- A move is described by a destination and a prefix.
    -- The move in algebraic notation is described by the grammar:
    --
    -- Move ::= Prefix <To : Coordinates_t> Suffix
    --
    -- <To : Coordinates_t> ::= <File : File_t> <Rank : Rank_t>
    --
    -- Prefix ::= From | From <Capture : Boolean>
    -- From ::= Piece | Piece <From : Disambiguating_Coordinates_t>
    -- <From : Disambiguating_Coordinates_t> ::=
    --   <File : File_t> | <Rank : Rank_t> | <Coords : Coordinates_t>
    -- Piece ::= Empty | <Piece : Piece_t>
    --
    -- Suffix ::= Empty | <Promotion : Piece_t>
    type Move_t(Castling : Castling_t := None) is
        record
            case Castling is
                when None =>
                    Piece     : Piece_t;
                    From      : Disambiguating_Coordinates_t;
                    Capture   : Boolean;
                    To        : Coordinates_t;
                    Promotion : Piece_t;
                when others =>
                    null;
            end case;
        end record;

    -- Enumeration of the possible results after a move is given by
    -- a player. Used in the function Move().
    -- TODO add more errors
    type MoveResult_t is (Valid_Move, Invalid_Move, Ambiguous_Move);

    -- The differents outcomes of a game
    type GameResult_t is (Playing,
                          Draw,
                          Check_White, Check_Black,
                          Checkmate_White, Checkmate_Black,
                          Resignation_White, Resignation_Black);


    Empty   : constant Cell_t := (IsEmpty => True);

    WKing   : constant Cell_t := (IsEmpty => False, Piece => King,   Color => White);
    WQueen  : constant Cell_t := (IsEmpty => False, Piece => Queen,  Color => White);
    WRook   : constant Cell_t := (IsEmpty => False, Piece => Rook,   Color => White);
    WBishop : constant Cell_t := (IsEmpty => False, Piece => Bishop, Color => White);
    WKnight : constant Cell_t := (IsEmpty => False, Piece => Knight, Color => White);
    WPawn   : constant Cell_t := (IsEmpty => False, Piece => Pawn,   Color => White);

    BKing   : constant Cell_t := (IsEmpty => False, Piece => King,   Color => Black);
    BQueen  : constant Cell_t := (IsEmpty => False, Piece => Queen,  Color => Black);
    BRook   : constant Cell_t := (IsEmpty => False, Piece => Rook,   Color => Black);
    BBishop : constant Cell_t := (IsEmpty => False, Piece => Bishop, Color => Black);
    BKnight : constant Cell_t := (IsEmpty => False, Piece => Knight, Color => Black);
    BPawn   : constant Cell_t := (IsEmpty => False, Piece => Pawn,   Color => Black);


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


    -- Move the piece on CurrMove.From to the position CurrMove.To
    -- on the given chess board Board.
    function Move(Board           : in out Board_t;
                  CurrMove        : in Move_t;
                  CurrPlayerColor : in Color_t) return MoveResult_t;

    -- Checks if the game has ended, either not, either someone wins
    -- or it is a tie.
    function Game_Ended(Board           : in Board_t;
                        CurrPlayerColor : in Color_t) return GameResult_t;


private


    -- Returns True if the cell is empty or belongs to an opponent
    function IsCellAccessible(Board       : in Board_t;
                              To          : in Coordinates_t;
                              PlayerColor : in Color_t;
                              Capture     : Boolean) return Boolean;

    -- Return True if the King at position From would be check at position To.
    function IsKingCheckAt(Board : in Board_t;
                           From  : in Coordinates_t;
                           To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = King);

    function IsKingCheck(Board : in Board_t;
                         To    : in Coordinates_t) return Boolean
      with Pre => (not Board(To.File, To.Rank).IsEmpty
                   and then Board(To.File, To.Rank).Piece = King);

    function IsKingCheckmate(Board : in Board_t;
                             To    : in Coordinates_t) return Boolean
      with Pre => (not Board(To.File, To.Rank).IsEmpty
                   and then Board(To.File, To.Rank).Piece = King);


    function IsValidMove_King(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = King);

    function IsValidMove_Queen(Board : in Board_t;
                               From  : in Coordinates_t;
                               To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = Queen);

    function IsValidMove_Rook(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = Rook);

    function IsValidMove_Bishop(Board : in Board_t;
                                From  : in Coordinates_t;
                                To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = Bishop);

    function IsValidMove_Knight(Board : in Board_t;
                                From  : in Coordinates_t;
                                To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = Knight);

    function IsValidMove_Pawn(Board : in Board_t;
                              From  : in Coordinates_t;
                              To    : in Coordinates_t) return Boolean
      with Pre => (not Board(From.File, From.Rank).IsEmpty
                   and then Board(From.File, From.Rank).Piece = Pawn);


    -- Returns True if the piece on From is allowed to move on To.
    function IsValidMove(Board   : in Board_t;
                         From    : in Coordinates_t;
                         To      : in Coordinates_t;
                         Capture : in Boolean) return Boolean
      with Pre => not Board(From.File, From.Rank).IsEmpty;


    -- Find a piece which fulfills the requirements from the given algebraic
    -- notation. If found the From coordinate will contain its location.
    function FindPiece(Board           : in  Board_t;
                       CurrMove        : in  Move_t;
                       CurrPlayerColor : in  Color_t;
                       From            : out Coordinates_t) return MoveResult_t;

end Board;
