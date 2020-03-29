with Ada.Text_IO, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Strings.Fixed;

procedure Day_22_part_2 is
    Input_File : String := "day_22.in";
    type Operation is record
        A : Long_Integer;
        B : Long_Integer;
    end record;
    M : Long_Integer := 119315717514047; -- assuming M is prime
    K : Long_Integer := 101741582076661;
    F : Operation;
    type Moves_Array is array (Positive range <>) of Operation;

    procedure Split(
        TheString : in String; Pos : in Integer;
        Part1 : out String; Part2 : out String) is
    begin
       Move(TheString(TheString'First .. Pos - 1), Part1);
       Move(TheString(Pos .. TheString'Last), Part2);
    end Split;

    function Parse_Operation(Line: in String) return Operation is
        Operation_Number : String := "                                  ";
        Operation_String : String := "                                  ";
        Number : Long_Integer := 0;
    begin
        if Line = "deal into new stack" then
            return (Long_Integer(-1), Long_Integer(-1));
        end if;
        if Line(Line'First .. Line'First + 3) = "cut " then
            Split(Line, 5, Operation_String, Operation_Number);
            Number := Long_Integer(Integer'Value(Operation_Number));
            return (Long_Integer(1), -Number);
        end if;
        if Line(Line'First .. Line'First + 19) = "deal with increment " then
            Split(Line, 21, Operation_String, Operation_Number);
            Number := Long_Integer(Integer'Value(Operation_Number));
            return (Number, Long_Integer(0));
        end if;
        raise Program_Error with "Unrecognized line occurred";
    end Parse_Operation;

    function Compose(Op1: in Operation; Op2: Operation) return Operation is
    begin
        return (Op1.a * Op2.a mod m , (Op1.b * Op1.a + Op2.b) mod M);
    end Compose;

    function Load_Operation(File: in String) return Operation is
        Input : File_Type;
        Op : Operation;
    begin
        Open (File => Input,
                Mode => In_File,
                Name => Input_File);
        Op := Parse_Operation(Get_Line (Input));
        loop
          declare
             Line : String := Get_Line(Input);
             Next_Op: Operation;
          begin
            Next_Op := Parse_Operation(Line);
            Op := Compose(Op, Next_Op);
            exit when End_Of_File(Input);
          end;
       end loop;
       Close (Input);
       return Op;
    end Load_Operation;
begin
    begin
      F := Load_Operation(Input_File);
      Put_Line(Long_Integer'Image(F.a) & " " & Long_Integer'Image(F.b));
    end;
end Day_22_part_2;

