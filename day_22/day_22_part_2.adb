with Ada.Text_IO, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Strings.Fixed;

procedure Day_22_part_2 is
    type Long is range -18..18;
    type Repeat is range 0..0;
    type Position is range 0..9;
    Input_File : String := "day_22.in";
    Start_Position : Position := 9;
    Card : Position := Start_Position;
    Input : File_Type;
    Command_Number : String := "                                  ";
    Command_String : String := "                                  ";
    Number : Long;
    CR : constant Character := Character'Val(13);
    Lines_Number : Positive;
    type Technique_Type is (Deal, Cut, Deal_Inc);
    type Command is record
        Technique : Technique_Type;
        Number : Long;
    end record;
    type Command_Array is array (Positive range <>) of Command;
    function Deal_Into_New_Stack(Card: in out Position) return Position is
    begin
        return Position'Last - Card;
    end Deal_Into_New_Stack;
    function Cut_N_Cards(Card: in out Position; Number: in Long) return Position is
        N: Position;
    begin
        If Number < Long(0) then -- Change negative cut into positive
            N := Position(Long(Position'Last) + Number + Long(1));
        else
            N := Position(Number);
        end if;
        If Card > Position'Last - N then
            return Card - Position'Last + N - 1;
        else
            return Card + N;
        end If;
    end Cut_N_Cards;
    function Deal_With_Increment (Card: in out Position; Number: in Long) return Position is
    begin
        return Position(
                            (Long(Card) *
                                ((Long(Position'Last) + Long(1)) - Number)
                            ) mod (Long(Position'Last) + Long(1))
                        );
    end Deal_With_Increment;
    procedure Split(
        TheString : in String; Pos : in Integer;
        Part1 : out String; Part2 : out String) is
    begin
       Move(TheString(TheString'First .. Pos - 1), Part1);
       Move(TheString(Pos .. TheString'Last), Part2);
    end Split;
    function Count_Lines(File: in String) return Long is
        Input : File_Type;
        Lines_Count : Long := 0;
    begin
        Open (File => Input,
            Mode => In_File,
            Name => Input_File);
        loop
            declare
               Line : String := Get_Line (Input);
            begin
                Lines_Count := Lines_Count + 1;
                exit when End_Of_File(Input);
            end;
        end loop;
        Close (Input);
        return Lines_Count;
    end Count_Lines;
begin
    Lines_Number := Positive(Count_Lines(Input_File));
    declare
        Commands : Command_Array (1..Lines_Number);
        I : Integer := 1;
    begin
        Open (File => Input,
            Mode => In_File,
            Name => Input_File);
        loop
          declare
             Line : String := Get_Line (Input);
          begin
            If Line = "deal into new stack" then
                Commands(I) := (Deal, 0);
            elsif Line(1 .. 4) = "cut " then
                Split(Line, 5, Command_String, Command_Number);
                Number := Long(Integer'Value(Command_Number));
                Commands(I) := (Cut, Number);
            elsif Line(1 .. 20) = "deal with increment " then
                Split(Line, 21, Command_String, Command_Number);
                Number := Long(Integer'Value(Command_Number));
                If Number = Long(1) or Number = Long(Position'Last) then
                        Number := Long(Position'Last) + 1 - Number;
                end If;
                Commands(I) := (Deal_Inc, Number);
            end If;
            I := I + 1;
            exit when End_Of_File(Input);
          end;
       end loop;
       Close (Input);
       For I in 0..0 loop
           For J in reverse Commands'Range loop
--                  Put_Line(Technique_Type'Image(Commands(J).Technique) & " " & Long'Image(Commands(J).Number));
                case Commands(J).Technique is
                    when Deal =>
                         Card := Deal_Into_New_Stack(Card);
                    when Cut =>
                         Card := Cut_N_Cards(Card, Commands(J).Number);
                    when Deal_Inc =>
                         Card := Deal_With_Increment(Card, Commands(J).Number);
                end case;
           end loop;
           Put_Line(Position'Image(Card));
       end loop;
    end;
    Put_Line(" ");
    Put(Position'Image(Card));
end Day_22_part_2;

