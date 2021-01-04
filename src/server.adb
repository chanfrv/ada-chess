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
	
	subtype Player_Index_t is Natural range 1 .. 2;
	type Players_t is array (Player_Index_t) of Player_t;

	Address    : Sock_Addr_Type;
	Server     : Socket_Type;
	Players    : Players_t;
	
	Finished   : Boolean;
	Board      : Board_t;
	CurrPlayer : Player_Index_t;
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
	Accept_Socket(Server, Players(1).Socket, Address);
	Put_Line("Accepted client 1");
	Players(1).Channel := Stream(Players(1).Socket);
	-- TODO Authenticate, assign color...
	-- Player 2
	Accept_Socket(Server, Players(2).Socket, Address);
	Put_Line("Accepted client 2");
	Players(2).Channel := Stream(Players(2).Socket);
	
	-- Play
	Board := StartBoard;
	Finished := False;
	CurrPlayer := 1; -- TODO find player with color white
	
	while not Finished loop
		--CurrMove := GetMove(String'Input(Players(CurrPlayer).Channel));
		MoveResult := Move(Board, CurrMove);
	
		case MoveResult is
		when Invalid_Move =>
		-- TODO send error to client
			null;
		when Success =>
			-- TODO broadcast move to the other player
			CurrPlayer := 3 - CurrPlayer;
		end case;
				
		Finished := True;
	end loop;
				
	-- Close
	Close_Socket(Server);
	Close_Socket(Players(1).Socket);
	Close_Socket(Players(2).Socket);
end Server;
