with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Board; use Board;

procedure Server is

	type Player_t is
	record
		Color   : Color_t;
		Socket  : Socket_Type;
		Channel : Stream_Access;
	end record;
	
	type Player_Access_t is access all Player_t;
		
	function GetMove(Input : String) return Move_t is
	begin
		-- TODO convert "a5" string to move record
		return ((A, 1), (B, 1));
	end GetMove;
	
	procedure BroadcastMove(Move : Move_t) is
	begin
		-- TODO when a player does a move, the server must propagate
		-- it to the other player
		null;
	end BroadcastMove;

	Address    : Sock_Addr_Type;
	Server     : Socket_Type;
	Player1    : aliased Player_t;
	Player2    : aliased Player_t;
	
	Finished   : Boolean;
	Board      : Board_t;
	CurrPlayer : Player_Access_t;
	CurrMove   : Move_t;
	MoveResult : MoveResult_t;
begin
	-- Connect
	Address.Addr := Addresses(Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket(Server);
	Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
	Bind_Socket(Server, Address);
	Listen_Socket(Server);
	Put_Line("Listening for connections");
	
	-- Player 1
	Accept_Socket(Server, Player1.Socket, Address);
	Put_Line("Accepted client 1");
	Player1.Channel := Stream(Player1.Socket);
	-- Authenticate, assign color...
	-- Player 2
	Accept_Socket(Server, Player2.Socket, Address);
	Put_Line("Accepted client 2");
	Player2.Channel := Stream(Player2.Socket);
	
	-- Play
	Board := StartBoard;
	Finished := False;
	CurrPlayer := Player1'Access;
	
	while not Finished loop
		CurrMove := GetMove(String'Input(CurrPlayer.Channel));
		MoveResult := Move(Board, CurrMove);
	
		case MoveResult is
		when Invalid_Move =>
		-- TODO send error to client
			null;
		when Success =>
			BroadcastMove(CurrMove);
			-- TODO change curr player
		end case;
				
		Finished := True;
	end loop;
				
	-- Close
	Close_Socket(Server);
	Close_Socket(Player1.Socket);
	Close_Socket(Player2.Socket);
end Server;
