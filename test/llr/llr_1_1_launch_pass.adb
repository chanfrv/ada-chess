with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Board.Strings.Pretty; use Board.Strings.Pretty;
with Server;
with Client;
with Board;


procedure llr_1_1_launch_pass is
    task Server_Launch is
        entry Start;
    end Server_Launch;
    
    task Client_1_Launch is
        entry Start;
    end Client_1_Launch;
    
    
    task body Server_Launch is
    begin
        accept Start;
        Server.Launch(5876, Black);
        OS_Exit(1);
    end Server_Launch;
    
    task body Client_1_Launch is
    begin
        accept Start;
        Client.Launch(Addresses(Get_Host_By_Name(Host_Name), 1), 5876, Black);
        OS_Exit(2);
    end Client_1_Launch;
    
begin
    Server_Launch.Start;
    delay(1.0);
    Client_1_Launch.Start;
    delay(1.0);
    
    OS_Exit(0);
end llr_1_1_launch_pass;
