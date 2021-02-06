with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
use  Ada.Text_IO;

with Board; use Board;


procedure llr_4_1_boardout_pass is
    
    task server is
        entry Start;
    end server;
    
    task client is
        entry Start;
    end client;
    
    task body server is
        sock : Socket_Type;
        address : Sock_Addr_Type;
        new_sock : Socket_Type;
        new_addr : Sock_Addr_Type;
    begin
        accept Start;
        
        address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
        address.Port := 5876;
        
        Create_Socket(sock);
        Set_Socket_Option(sock, Socket_Level, (Reuse_Address, True));
        Bind_Socket(sock, address);
        Listen_Socket(sock);
        Accept_Socket(sock, new_sock, new_addr);
        
        Board_t'Output(Stream(new_sock), StartBoard);
        
        delay(1.0); -- wait 1s for the client to receive
    
        Close_Socket(new_sock);
        Close_Socket(sock);
    end server;
    
    task body client is
        sock : Socket_Type;
        address : Sock_Addr_Type;
        
        board : Board_t;
    begin
        accept Start;
        
        address.Addr := Addresses(Get_Host_By_Name(Host_Name), 1);
        address.Port := 5876;
        
        Create_Socket(sock);
        Set_Socket_Option(sock, Socket_Level, (Reuse_Address, True));
        Connect_Socket(sock, address);
        
        board := Board_t'Input(Stream(sock));
        
        Close_Socket(sock);
                
        if board /= StartBoard then
            OS_Exit(1);
        end if;
    end client;
        
begin
    server.Start;
    delay(1.0);
    client.Start;
end llr_4_1_boardout_pass;
