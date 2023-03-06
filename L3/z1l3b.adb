with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1;

use Ada.Containers;
use Ada.Strings.Unbounded;
use Ada.Characters.Latin_1;


procedure z1l3b is

   Bound : Integer := 3;

   type randRange is new Integer range 0..10000;


   package Rand_Int is new Ada.Numerics.Discrete_Random(randRange);
   use Rand_Int;

   package Integer_Vectors is new Vectors(Natural, Integer);

   subtype Idx is Integer range 0..100;
   type ArrOfVector is array (Idx) of Integer_Vectors.Vector;

   type Pair is
      record
         J : Integer;
         Cost : Integer;
       end record;

   package Pair_Vectors is new Vectors(Natural, Pair);

   type Triple is
      record
         Nexthop : Integer;
         Cost    : Integer;
         Changed : Boolean;
      end record;

   type Table is array(Idx) of Triple;

   type Packet is
      record
         L : Integer;
         P : Pair_Vectors.Vector;
      end record;


   function Search(A : Integer_Vectors.Vector ; X : Integer) return Boolean is

      Len : Integer;

    begin

      Len := Integer(A.Length);
      for I in 0..(Len-1) loop
         if A(I) = X then
            return True;
         end if;
      end loop;

      return False;
   end Search;

   protected type T is

      procedure Init(N : Integer ; I : Integer ; MyNeighbours : Integer_Vectors.Vector);
      procedure SenderInfo(N : Integer ; MyId : Integer ; New_Packet : out Packet);
      procedure ReceiverSet(P : Packet ; MyId : Integer);
      procedure GetCurrentState(N : Integer ; St : out Unbounded_String);

   private

      RT : Table;

   end T;

   function packetToString(P : Packet) return Unbounded_String is
      Res : Unbounded_String := To_Unbounded_String("");
   begin

      for I in P.P.First_Index..P.P.Last_Index loop
         Res := Res & To_Unbounded_String("( ") & Integer'Image(P.P(I).J) & To_Unbounded_String(", ")
           & Integer'Image(P.P(I).Cost) & To_Unbounded_String(") ");
      end loop;

      return Res;

   end packetToString;

   function GetChangedFlags(P : Packet) return Unbounded_String is
      Res : Unbounded_String := To_Unbounded_String("");
   begin

      for I in P.P.First_Index..P.P.Last_Index loop
         Res := Res & Integer'Image(P.P(I).J) & To_Unbounded_String(" ");
      end loop;

      return Res;

   end GetChangedFlags;


   protected body T is

      procedure Init(N : Integer ; I : Integer ; MyNeighbours : Integer_Vectors.Vector) is
      begin

         for J in 0..(N-1) loop

            if I /= J then

              if Search(MyNeighbours, J) then

                 RT(J).Nexthop := J;
                 RT(J).Cost := 1;
                 RT(J).Changed := True;

              else

                 RT(J).Cost := abs(I - J);
                 RT(J).Changed := True;

                 if I < J then

                   RT(J).Nexthop := I+1;

                 else

                   RT(J).Nexthop := I-1;

                 end if;

              end if;

            end if;


         end loop;

      end Init;


      procedure SenderInfo(N : Integer ; MyId : Integer ; New_Packet : out Packet) is
             New_Pair : Pair;
             P : Pair_Vectors.Vector;
      begin

         New_Packet.L := MyId;

         for I in 0..(N-1) loop

            if MyId /= I then

               if RT(I).Changed then

                  New_Pair := Pair'(J => I, Cost => RT(I).Cost);
                  New_Packet.P.Append(New_Pair);

                  RT(I).Changed := False;

               end if;

            end if;

         end loop;


      end SenderInfo;

      procedure ReceiverSet(P : Packet ; MyId : Integer) is
         New_Cost : Integer;
                L : Integer;
         V : Pair_Vectors.Vector;
         J : Integer;

      begin

         L := P.L;
         V := P.P;

         for I in V.First_Index..V.Last_Index loop

            New_Cost := V(I).Cost + 1;
            J := V(I).J;

            if J /= MyId then

                if New_Cost < RT(J).Cost then

                  RT(J).Cost := New_Cost;
                  RT(J).Nexthop := L;
                  RT(J).Changed := True;

               end if;

            end if;

         end loop;

      end ReceiverSet;

      procedure GetCurrentState(N : Integer ; St : out Unbounded_String) is
      begin
         St := To_Unbounded_String("");
         for I in 0..N-1 loop
            St := St & Integer'Image(I) & To_Unbounded_String(" - ") & To_Unbounded_String("(") & Integer'Image(RT(I).Nexthop) &
              To_Unbounded_String(", ") & Integer'Image(RT(I).Cost) & To_Unbounded_String(", ")
              & Boolean'Image(RT(I).Changed) & To_Unbounded_String(") ");
         end loop;
      end GetCurrentState;


   end T;


   type T_Arr is array(Idx) of T;
   Prot_Arr : T_Arr;

   task Server is
       entry Acc(Message : Unbounded_String);
   end Server;


   task type Receiver is
      entry Init(MyId : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector);
      entry Receive(P : Packet);
   end Receiver;

   task body Receiver is
      Id : Integer;
      MyN : Integer;
      MyNeighbours : Integer_Vectors.Vector;
      MyPacket : Packet;
      Msg : Unbounded_String;
      StateStr : Unbounded_String := To_Unbounded_String("");
   begin

      accept Init(MyId : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector ) do
         Id := MyId;
         MyN := N;
         MyNeighbours := Neighbours;
      end Init;


      loop
         select

            accept Receive (P : in Packet) do
               MyPacket := P;
            end Receive;

            Msg := To_Unbounded_String("Receiver ") & Integer'Image(Id) &
              To_Unbounded_String("- ") & To_Unbounded_String("Otrzymalem pakiet: ") &
              packetToString(MyPacket) & To_Unbounded_String(" Od : ") & Integer'Image(MyPacket.L) ;

            Prot_Arr(Id).GetCurrentState(N => MyN, St => StateStr);

            Msg := Msg & CR & LF & To_Unbounded_String("Receiver ") & Integer'Image(Id) & To_Unbounded_String("- ") &
              To_Unbounded_String("stan przed zmiana: ") & StateStr;

            Prot_Arr(Id).ReceiverSet(MyPacket, Id);

            Prot_Arr(Id).GetCurrentState(N => MyN, St => StateStr);

            Msg := Msg & CR & LF & To_Unbounded_String("Receiver ") & Integer'Image(Id) & To_Unbounded_String("- ") &
             To_Unbounded_String("stan po zmianie: ") & StateStr;
           Server.Acc(Msg);

         or
            terminate;

         end select;
      end loop;


   end Receiver;


   type RecArr is array(Idx) of Receiver;
   RArr : RecArr;


   task type Sender is
       entry Init(Id : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector );
   end Sender;

   task body Sender is
      MyId : Integer;
      MyN : Integer;
      MyNeighbours : Integer_Vectors.Vector;
      FloatGen : Ada.Numerics.Float_Random.Generator;
      IntGen : Generator;
      MyPacket : Packet;
      Msg : Unbounded_String;
      StateStr : Unbounded_String;
   begin

      Ada.Numerics.Float_Random.Reset(FloatGen);
      Reset(IntGen);

      accept Init(Id : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector ) do

         MyId := Id;
         MyN := N;
         MyNeighbours := Neighbours;

      end Init;

      loop


         delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(FloatGen));

         Prot_Arr(MyId).GetCurrentState(N => MyN, St => StateStr);

         Msg := To_Unbounded_String("Sender ") & Integer'Image(MyId) & To_Unbounded_String("- ") &
            To_Unbounded_String("stan przed zmiana: ") & StateStr;

         Prot_Arr(MyId).SenderInfo(MyN, MyId, MyPacket);

         Prot_Arr(MyId).GetCurrentState(N => MyN, St => StateStr);

         Msg := Msg & CR & LF & To_Unbounded_String("Sender ") & Integer'Image(MyId) & To_Unbounded_String("- ") &
              To_Unbounded_String("stan po zmianie: ") & StateStr;

         if MyPacket.P.Length > 0 then

            Msg := Msg & CR & LF & To_Unbounded_String("Sender ") & Integer'Image(MyId) &
              To_Unbounded_String("- ") & To_Unbounded_String("Wysylam pakiet: ") & packetToString(MyPacket);
            Server.Acc(Msg);

            for I in MyNeighbours.First_Index..MyNeighbours.Last_Index loop
               RArr(MyNeighbours(I)).Receive(MyPacket);
            end loop;

            MyPacket.P.Clear;

         else

             Msg := Msg & CR & LF & To_Unbounded_String("Sender ") & Integer'Image(MyId) &
              To_Unbounded_String("- ") & To_Unbounded_String("zadna zmienna nie miala flagi ustawionej na true ");
            Server.Acc(Msg);

         end if;

      end loop;


   end Sender;

   task type Vertice is
       entry Init(Id : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector);
   end Vertice;

   task body Vertice is
      MyId : Integer;
      MyN : Integer;
      MyNeighbours : Integer_Vectors.Vector;
      MySender : Sender;
   begin

      accept Init (Id : in Integer; N : in Integer; Neighbours : in Integer_Vectors.Vector) do
         MyId := Id;
         MyN := N;
         MyNeighbours := Neighbours;
      end Init;


      Prot_Arr(MyId).Init(MyN, MyId, MyNeighbours);

      MySender.Init(Id         => MyId,
                    N          => MyN,
                    Neighbours => MyNeighbours
                   );

      RArr(MyId).Init(MyId       => MyId,
                        N          => MyN,
                        Neighbours => MyNeighbours
                        );

      loop
         null;
      end loop;

   end Vertice;


   task body Server is
      MyMsg : Unbounded_String;
   begin
      loop
         select

            accept Acc(Message : Unbounded_String) do
               MyMsg := Message;

            end Acc;

            Ada.Text_IO.Unbounded_IO.Put(MyMsg);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Unbounded_IO.Put_Line(To_Unbounded_String("- - - - - - - - - - -"));
         or
            terminate;

         end select;
      end loop;
   end Server;

   N : Integer;
   D : Integer;
   V1 : Integer;
   V2 : Integer;
   Len : Integer;
   NArr : ArrOfVector;
   IntGen : Generator;


begin

   Ada.Text_IO.Put("Wprowadz liczbe wierzcholkow w grafie: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(N);

   Ada.Text_IO.Put("Wprowadz liczbe skrotow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(D);

   Reset(IntGen);

   for I in 0..N-2 loop

      NArr(I).Append(I+1);
      NArr(I+1).Append(I);

   end loop;

   for I in 0..D-1 loop

      while True loop

         V1 := Integer(Random(IntGen)) mod N;
         V2 := Integer(Random(IntGen)) mod N;

         exit when Search(NArr(V1), V2) = False and V1 /= V2;

      end loop;

      NArr(V1).Append(V2);
      NArr(V2).Append(V1);

   end loop;

   Ada.Text_IO.Put("Graf wygenerowany. Drukuje liste sasiedztwa...");
   Ada.Text_IO.New_Line;


   for I in 0..(N-1) loop
      Ada.Text_IO.Put("Wierzcholek: " & Integer'Image(I));
      Len := Integer(NArr(I).Length);
      Ada.Text_IO.Put(" | Sasiedzi: ");
      for J in 0..(Len-1) loop

         Ada.Integer_Text_IO.Put(NArr(I)(J));

      end loop;

      Ada.Text_IO.New_Line;

   end loop;

   Ada.Text_IO.Put("Rozpoczynam symulacje...");
   Ada.Text_IO.New_Line;

   declare
      type VArr is array(0..N-1) of Vertice;
      A : VArr;
   begin

      for I in 0..N-1 loop
         A(I).Init(I, N, NArr(I));
      end loop;

   end;

   loop
      null;
   end loop;

end z1l3b;

