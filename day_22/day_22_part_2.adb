with Ada.Text_IO, Ada.Strings.Fixed;
use Ada.Text_IO, Ada.Strings.Fixed;

procedure Day_22_part_2 is
    Input_File : String := "day_22.in";
    type Long_AS is range -(2 ** 63) .. +(2 ** 63 - 1);
    type Operation is record
        A : Long_AS;
        B : Long_AS;
    end record;
    M : Long_AS := 119315717514047; -- assuming M is prime
    K : Long_AS := 101741582076661;
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
        Number : Long_AS := 0;
    begin
        if Line = "deal into new stack" then
            return (Long_AS(-1), Long_AS(-1));
        end if;
        if Line(Line'First .. Line'First + 3) = "cut " then
            Split(Line, 5, Operation_String, Operation_Number);
            Number := Long_AS'Value(Operation_Number);
            return (Long_AS(1), -Number);
        end if;
        if Line(Line'First .. Line'First + 19) = "deal with increment " then
            Split(Line, 21, Operation_String, Operation_Number);
            Number := Long_AS'Value(Operation_Number);
            return (Number, Long_AS(0));
        end if;
        raise Program_Error with "Unrecognized line occurred";
    end Parse_Operation;

    function Mul_Mod(Input_A: Long_AS; Input_B : Long_AS; M: Long_AS) return Long_AS is
        Y : Long_AS := 0;
        A : Long_AS := Abs(Input_A);
        B : Long_AS := Abs(Input_B);
        Negative : Boolean := (Input_A >= 0) xor (Input_B >= 0);
    begin
        while B /= 0 loop
            if B mod 2 /= 0 then
                Y := (Y + A) mod M;
            end if;
            B := B / 2;
            A := (2 * A) mod M;
        end loop;
        if Negative = True then
            Y := (-Y) mod M;
        end if;
        return Y;
    end Mul_Mod;

    function Compose(Op1: in Operation; Op2: in Operation) return Operation is
    begin
        return (Mul_Mod(Op1.a, Op2.a, M), (Mul_Mod(Op1.b, Op2.a, M) + Op2.b) mod M);
    end Compose;

    function Load_Operation(File: in String) return Operation is
        Input : File_Type;
        Op : Operation;
    begin
        Open (File => Input,
                Mode => In_File,
                Name => Input_File);
        Op := Parse_Operation(Get_Line(Input));
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
    function Pow_Mod(Input_X: Long_AS; Input_N:Long_AS; M:Long_AS) return Long_AS is
        Y : Long_AS := 1;
        N : Long_AS := Input_N;
        X : Long_AS := Input_X;
    begin
        while N > 0 loop
            if n mod 2 /= 0 then
                Y := Mul_Mod(Y, X, M);
            end if;
            N := N / 2;
            X := Mul_Mod(X, X, M);
        end loop;
        return Y;
    end Pow_Mod;
    function Inverse_Mod(A:Long_AS; M: Long_AS) return Long_AS is
    begin
        return Pow_Mod(A, M - 2, M);
    end Inverse_Mod;
    function F_Repeated_K_Times_Inverse(F: Operation; K: Long_AS; X: Long_AS) return Long_AS is
        A : Long_AS := Pow_Mod(F.A, K, M);
        B : Long_AS := Mul_Mod(Mul_Mod(F.B, (1 - A), M), Inverse_Mod(1 - F.A, M), M);
    begin
        return Mul_Mod((X - B), Inverse_Mod(A, M), M);
    end F_Repeated_K_Times_Inverse;
begin
    begin
      F := Load_Operation(Input_File);
      Put_Line(Long_AS'Image(F_Repeated_K_Times_Inverse(F, K, 2020)));
    end;
end Day_22_part_2;

