with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Board; use Board;


procedure Client is
	Address  : Sock_Addr_Type;
	Socket   : Socket_Type;
	Channel  : Stream_Access;
		
	Finished : Boolean;
	Str      : String(1 .. 80);
	Last     : Natural;
begin
	-- Connect
	Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5876;
	Create_Socket (Socket);
	Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
	Connect_Socket (Socket, Address);
	Channel := Stream (Socket);
	Finished := False;
	
	-- Authenticate, choose piece color...
	-- TODO
		
	-- Play
	while not Finished loop
		-- Get and send move
		Get_Line(Str, Last);
		String'Output (Channel, Str);
	
		-- Receive response (success, error, you win...)
		Put_Line(String'Input(Channel));
		Finished := True;
	end loop;
		
	-- Disconnect
	Close_Socket (Socket);
end Client;
