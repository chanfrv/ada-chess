package board is

	type File_t is (A, B, C, D, E, F, G, H);
	subtype Rank_t is Positive range 1 .. 8;
	
	type Coordinates_t is
		record
			File : File_t;
			Rank : Rank_t;
		end record;
				
	type Move_t is
		record
			From : Coordinates_t;
			To   : Coordinates_t;
		end record;
	
		
	type Color_t is (White, Black);
	type Piece_t is (King, Queen, Rook, Bishop, Knight, Pawn);

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
	
		
	-- TODO add more errors
	type MoveResult_t is (Success, Invalid_Move);
		
		
	Empty   : constant BoardPiece_T := (IsEmpty => True);
	
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
	
	
	StartBoard : constant Board_t :=
	  ((BRook, BKnight, BBishop, BQueen, BKing, BBishop, BKnight, BRook),
	(BPawn, BPawn, BPawn, BPawn, BPawn, BPawn, BPawn, BPawn),
	(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
	(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
	(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
	(Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty),
	(WPawn, WPawn, WPawn, WPawn, WPawn, WPawn, WPawn, WPawn),
	(WRook, WKnight, WBishop, WQueen, WKing, WBishop, WKnight, WRook));
	
	
	function Move(Board : in out Board_t; CurrMove : in Move_t) return MoveResult_t;
	
	function Game_Ended(Board : in Board_t) return Boolean;
	
end board;
