with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;


package body Board.Strings.Pretty is

    
    procedure Print_Files_Left(File : in File_t; Rank : in Rank_t) is
    begin
        if File = a then
            Put(" " & Image(Rank) & " ");
        end if;
    end Print_Files_Left;
        
    procedure Print_Files_Right(File       : in File_t;
                                Rank       : in Rank_t)
    is
    begin
        if File = h then
            Put(ASCII.ESC & "[0m " & Image(Rank));
        end if;
    end Print_Files_Right;
        
    function Get_Square(File : in File_t; Rank : in Rank_t) return String
    is        
        Square_White : String := ASCII.ESC & "[47m" & ASCII.ESC & "[1;30m";
        Square_Black : String := ASCII.ESC & "[40m" & ASCII.ESC & "[1;37m";
    begin
        return (if (Rank + File_t'Pos(File) + 1) mod 2 = 0 then
                    Square_White
                else
                    Square_Black);
    end Get_Square;
    
    
    procedure Pretty_Print(Board : in Board_t)
    is
        Cell       : Cell_t;
        Background : String(1 .. 12);
    begin
        -- Top ranks
        Put_Line("    a  b  c  d  e  f  g  h");
        
        for Rank in 1 .. 8 loop
              
            for File in a .. h loop
                -- Left files
                Print_Files_Left(File, Rank);
                
                -- Background color
                Background := Get_Square(File, Rank);
                
                -- Foreground symbol
                Cell := Board(File, Rank);
                
                -- Print the cell
                Put(Background & " " & Image(Cell) & " ");
                
                -- Right files
                Print_Files_Right(File, Rank);
            end loop;
            
            New_Line;
            
        end loop;
        
        -- Bottom ranks
        Put_Line("    a  b  c  d  e  f  g  h");
        
    end Pretty_Print;
    
    
    procedure Pretty_Print(Board : in Board_t; CurrPlayer : in Color_t)
    is
        Cell        : Cell_t;
        Background  : String(1 .. 12);
        
        Token_White : String := (if CurrPlayer = White then "➤" else " ");
        Token_Black : String := (if CurrPlayer = Black then "➤" else " ");
    begin
        -- Top ranks
        Put_Line("    a  b  c  d  e  f  g  h");

        for Rank in 1 .. 8 loop
            
            for File in a .. h loop
                -- Left files
                Print_Files_Left(File, Rank);
                
                -- Background color
                Background := Get_Square(File, Rank);
                
                -- Foreground symbol
                Cell := Board(File, Rank);
                
                -- Print the cell
                Put(Background & " " & Image(Cell) & " ");
                
                -- Right files
                Print_Files_Right(File, Rank);
                
                case Rank is
                    when 2 => Put("    " & Token_White & " White");
                    when 7 => Put("    " & Token_Black & " Black");
                    when others => null;
                end case;
            end loop;
            
            New_Line;
            
        end loop;
        
        -- Bottom ranks
        Put_Line("    a  b  c  d  e  f  g  h");

    end Pretty_Print;
    

end Board.Strings.Pretty;
