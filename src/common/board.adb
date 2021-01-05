package body board is

	function Move(Board : in out Board_t; CurrMove : in Move_t) return MoveResult_t is
	begin
		-- TODO check valid move etc then move
		return Success;
	end Move;

	function Game_Ended(Board : in Board_t) return Boolean is
	begin
		-- TODO
		return False;
	end Game_Ended;

end board;
