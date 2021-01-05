with Ada.Characters.Handling; use Ada.Characters.Handling;


package body Communication is

    function Image(Id : Command) return String is
    begin
        return To_Lower(Id'Image);
    end Image;
    
    function Image(Cmd : CommandLine) return String is
    begin
        return Image(Cmd.Id)
          & (
             if Cmd.Id = Move then
                 ";" & Image(Cmd.Move)
             else
                 ""
            );
    end Image;
    
    
    procedure Send(Channel : Stream_Access; Cmd : CommandLine) is
    begin
        String'Output(Channel, Image(Cmd));
    end Send;

end Communication;
