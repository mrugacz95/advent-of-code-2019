with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
use Ada.Containers;

procedure Day_22 is
    type Card is range 0..9;
    type Deck_Type is array (0..9) of Card;
    Deck : Deck_Type;
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
        Put_Line(Integer'Image(N));
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
begin
    For I in Deck'Range loop
        Deck(I) := Card(I);
    end loop;
    Deck := Deal_With_Increment(Deck, 7);
    Deck := Deal_With_Increment(Deck, 9);
    Deck := Cut_N_Cards(Deck, -2);
    For I in Deck'Range loop
        Put_Line( Integer'Image(I) & "" & Card'Image(Deck(I)));
    end loop;
end Day_22;

