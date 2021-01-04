with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;


procedure Server is
	Address  : Sock_Addr_Type;
	Server   : Socket_Type;
	Socket   : Socket_Type;
	Channel  : Stream_Access;
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
	Put_Line(String'Input(Channel));
	
	-- Close
	Close_Socket(Server);
	Close_Socket(Socket);
end Server;
