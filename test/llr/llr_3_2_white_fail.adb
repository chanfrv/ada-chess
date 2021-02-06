with GNAT.Sockets; use GNAT.Sockets;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Server; use Server;
with Board; use Board;


procedure llr_3_2_white_fail is
    p1 : Player_t := (2, White, No_Socket, null);
    p2 : Player_t := (1, Black, No_Socket, null);
    ps : Players_t := (p1, p2);
begin
    if GetWhitePlayer(ps) = 1 then
        OS_Exit(0);
    else
        OS_Exit(1);
    end if;        
end llr_3_2_white_fail;
