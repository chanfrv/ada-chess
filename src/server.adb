with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Board; use Board;

procedure Server is

	function GetMove(Input : String) return Move_t is
	begin
		-- TODO read move from network
		return ((A, 1), (B, 1));
	end GetMove;

	Address  : Sock_Addr_Type;
	Server   : Socket_Type;
	Socket   : Socket_Type;
	Channel  : Stream_Access;
	
	Board    : Board_t;
	CurrMove : Move_t;
begin
	-- Connect
	Address.Addr := Addresses(Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket(Server);
	Set_Socket_Option(Server, Socket_Level, (Reuse_Address, True));
	Bind_Socket(Server, Address);
	Listen_Socket(Server);
	Accept_Socket(Server, Socket, Address);
	Channel := Stream(Socket);
	
	-- Communication
	Board := StartBoard;
	
	CurrMove := GetMove(String'Input(Channel));
	Move(Board, CurrMove);
	
	-- Close
	Close_Socket(Server);
	Close_Socket(Socket);
end Server;
