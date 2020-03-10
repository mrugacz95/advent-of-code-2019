with Ada.Text_IO, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Strings.Fixed;

procedure Day_22 is
    type Card is range 0..10006;
    type Deck_Type is array (0..10006) of Card;
    Deck : Deck_Type;
    Input : File_Type;
    Command_Number : String := "        ";
    Command : String := "                                  ";
    Number : Integer;
    function Deal_Into_New_Stack(Deck: in Deck_Type) return Deck_Type is
        Tmp_Deck : Deck_Type;
    begin
        For I in Deck'Range loop
            Tmp_Deck(I) := Deck(Deck'Last - I);
        end loop;
        return Tmp_Deck;
    end Deal_Into_New_Stack;
    function Cut_N_Cards(Deck: in Deck_Type; Number: in Integer) return Deck_Type is
        N: Integer := Number;
        Tmp_Deck : Deck_Type;
    begin
        If Number < 0 then
            N := Deck_Type'Last + Number + 1;
        end if;
        For I in Deck'First..Deck'Last - N loop
            Tmp_Deck(I) := Deck(I + N);
        end loop;
        For I in 0..N - 1 loop
            Tmp_Deck(Deck'Last - I) := Deck(N - I - 1);
        end loop;
        return Tmp_Deck;
    end Cut_N_Cards;
    function Deal_With_Increment (Deck: in Deck_Type; Number: in Integer) return Deck_Type is
        Tmp_Deck : Deck_Type;
        N : Integer := 0;
    begin
        For I in Deck'Range loop
            Tmp_Deck(N) := Deck(I);
            N := N + Number;
            N := N mod (Deck'Last + 1);
        end loop;
        return Tmp_Deck;
    end Deal_With_Increment;
    function Prepare_Deck return Deck_Type is
    begin
        For I in Deck'Range loop
            Deck(I) := Card(I);
        end loop;
        return Deck;
    end Prepare_Deck;
    procedure Split(
        TheString : in String; Pos : in Integer;
        Part1 : out String; Part2 : out String) is
    begin
       Move(TheString(TheString'First .. Pos - 1), Part1);
       Move(TheString(Pos .. TheString'Last), Part2);
    end Split;
begin
    Deck := Prepare_Deck;
    Open (File => Input,
        Mode => In_File,
        Name => "day_22.in");
    loop
      declare
         Line : String := Get_Line (Input);
      begin
        If Line = "deal into new stack" then
            Deck := Deal_Into_New_Stack(Deck);
--              Put_Line("deal into new stack");
        elsif Line(1 .. 4) = "cut " then
            Split(Line, 5, Command, Command_Number);
            Number := Integer'Value(Command_Number);
--              Put_Line("cut " & Integer'Image(Number));
            Deck := Cut_N_Cards(Deck, Number);
        elsif Line(1 .. 20) = "deal with increment " then
            Split(Line, 21, Command, Command_Number);
            Number := Integer'Value(Command_Number);
--              Put_Line("deal with increment" & Integer'Image(Number));
            Deck := Deal_With_Increment(Deck, Number);
        end If;
        exit when End_Of_File(Input);
      end;
   end loop;
   Close (Input);
   For I in Deck'Range loop
        If Deck(I) = 2019 then
            Put(Integer'Image(I));
            exit;
        end If;
   end loop;
end Day_22;

