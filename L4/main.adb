with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Latin_1;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

use Ada.Containers;
use Ada.Strings.Unbounded;
use Ada.Characters.Latin_1;


procedure Main is

   Bound : Integer := 2;

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

   type Addr is record
      R : Integer;
      H : Integer;
   end record;

   type Std_Packet is record
      S : Addr;
      R : Addr;
      Visited : Integer_Vectors.Vector;
   end record;

   package Packet_Queue_Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type => Std_Packet);
   package Packets_Queue is new Ada.Containers.Unbounded_Synchronized_Queues(Queue_Interfaces => Packet_Queue_Interfaces
                                                                            );


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
      procedure ReceiverSet(P : Packet ; MyId : Integer ; Flag : in out Boolean);
      procedure GetCurrentState(N : Integer ; St : out Unbounded_String);
      function GetNexthop(Rec_R : Integer) return Integer;

   private

      RT : Table;

   end T;

   function stdPacketToString(P : Std_Packet) return Unbounded_String is
      Res : Unbounded_String := To_Unbounded_String("");
   begin

      declare
         Sender_R : Integer;
         Sender_H : Integer;
         Receiver_R : Integer;
         Receiver_H : Integer;
         S_Half : Unbounded_String := To_Unbounded_String("");
      begin

         Sender_R := P.S.R;
         Sender_H := P.S.H;
         Receiver_H := P.R.H;
         Receiver_R := P.R.R;

         Res := To_Unbounded_String("Nadawca : ") & To_Unbounded_String("(") & Integer'Image(Sender_R) &
           To_Unbounded_String(" ,") & Integer'Image(Sender_H) & To_Unbounded_String(") ; ") &
           To_Unbounded_String("Odbiorca : ") & To_Unbounded_String("(") & Integer'Image(Receiver_R) &
           To_Unbounded_String(" ,") & Integer'Image(Receiver_H) & To_Unbounded_String(")") & CR & LF;

         for I in P.Visited.First_Index..P.Visited.Last_Index loop

            S_Half := S_Half & Integer'Image(P.Visited(I)) & To_Unbounded_String(" ");

         end loop;

         Res := Res & S_Half;

         return Res;

      end;

   end stdPacketToString;


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

      procedure ReceiverSet(P : Packet ; MyId : Integer ; Flag : in out Boolean) is
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

                  Flag := True;

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

      function GetNexthop(Rec_R : Integer) return Integer is
      begin
         return RT(Rec_R).Nexthop;
      end GetNexthop;


   end T;


   type T_Arr is array(Idx) of T;
   Prot_Arr : T_Arr;

   type QueueArray is array(Idx) of Packets_Queue.Queue;
   QArr : QueueArray;

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
      MyFlag : Boolean := False;
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

            Prot_Arr(Id).ReceiverSet(MyPacket, Id, MyFlag);

            Msg := To_Unbounded_String("");

            if MyFlag then

               Prot_Arr(Id).GetCurrentState(N => MyN, St => StateStr);

               Msg := Msg & CR & LF & To_Unbounded_String("Receiver ") & Integer'Image(Id) & To_Unbounded_String("- ") &
                 To_Unbounded_String("stan po zmianie: ") & StateStr;
               Server.Acc(Msg);

               MyFlag := False;

            end if;

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
      MyPacket : Packet;
      Msg : Unbounded_String;
   begin

      Ada.Numerics.Float_Random.Reset(FloatGen);


      accept Init(Id : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector ) do

         MyId := Id;
         MyN := N;
         MyNeighbours := Neighbours;

      end Init;

      loop

         delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(FloatGen));

         Prot_Arr(MyId).SenderInfo(MyN, MyId, MyPacket);


         if MyPacket.P.Length > 0 then

            for I in MyNeighbours.First_Index..MyNeighbours.Last_Index loop
               RArr(MyNeighbours(I)).Receive(MyPacket);
            end loop;

            MyPacket.P.Clear;

         end if;

      end loop;


   end Sender;

   task type Receiving_Forwarder is
      entry Init(R_Id : Integer);
      entry Receive(SPacket : Std_Packet);
   end Receiving_Forwarder;

   task type Sending_Forwarder is
      entry Init(R_Id : Integer);
   end Sending_Forwarder;

    task type Host is
      entry Init(R_Id : Integer ; H_Id : Integer ; Neighbours : Integer_Vectors.Vector ; E_Hosts : Integer_Vectors.Vector);
      entry Receive_Packet(P : Std_Packet);
   end Host;

   type Host_Array is array(0..19, 0..19) of Host;
   HArr : Host_Array;

   type RForwardersArray is array(Idx) of Receiving_Forwarder;
   RForwardersArr : RForwardersArray;

   task body Receiving_Forwarder is
      MyRId : Integer;
      RPacket : Std_Packet;
   begin

      accept Init(R_Id : Integer) do

          MyRId := R_Id;

      end Init;

      loop
         select

            accept Receive (SPacket : in Std_Packet) do
               RPacket := SPacket;
            end Receive;

            RPacket.Visited.Append(MyRId);
            QArr(MyRId).Enqueue(New_Item => RPacket);

         or
            terminate;

         end select;

      end loop;

   end Receiving_Forwarder;


   task body Sending_Forwarder is
      MyPacket : Std_Packet;
      MyQueue : Packets_Queue.Queue;
      MyRId : Integer;

   begin

      accept Init (R_Id : in Integer) do

         MyRId := R_Id;

      end Init;

      loop

         QArr(MyRId).Dequeue(Element => MyPacket);

         if MyPacket.R.R = MyRId then

            HArr(MyRId, MyPacket.R.H).Receive_Packet(P => MyPacket);

         else

            declare

               Nexthop : Integer;

            begin

               Nexthop := Prot_Arr(MyRId).GetNexthop(Rec_R => MyPacket.R.R);

               RForwardersArr(Nexthop).Receive(MyPacket);

            end;


         end if;

      end loop;

   end Sending_Forwarder;

   task body Host is
      MyR_Id : Integer;
      MyH_Id : Integer;
      MyNeighbours : Integer_Vectors.Vector;
      RGen : Ada.Numerics.Float_Random.Generator;
      IGen : Generator;
      MyE_Hosts : Integer_Vectors.Vector;
       SR : Integer;
      SH : Integer;
      My_P : Std_Packet;
      To_Send : Std_Packet;
      S_Addr : Addr;
      R_Addr : Addr;
      Vr : Integer_Vectors.Vector;
   begin

      Ada.Numerics.Float_Random.Reset(RGen);
      Reset(IGen);

      accept Init (R_Id : in Integer; H_Id : in Integer; Neighbours : in Integer_Vectors.Vector ; E_Hosts : Integer_Vectors.Vector) do

         MyR_Id := R_Id;
         MyH_Id := H_Id;
         MyNeighbours := Neighbours;
         MyE_Hosts := E_Hosts;

      end Init;

      declare

         L : Integer;

      begin

         L := Integer(MyE_Hosts.Length);

         while True loop

            SR := Integer(Random(IGen)) mod L;
            SH := Integer(Random(IGen)) mod (MyE_Hosts(SR));

            exit when SR /= MyR_Id or SH /= MyH_Id;

         end loop;

      end;


      declare

         Sender_Addr : Addr;
         Receiver_Addr : Addr;
         New_Packet : Std_Packet;
         Visited_Routers : Integer_Vectors.Vector;

      begin

         Sender_Addr := Addr'(MyR_Id, MyH_Id);
         Receiver_Addr := Addr'(SR, SH);

         New_Packet := Std_Packet'(Sender_Addr, Receiver_Addr, Visited_Routers);

         RForwardersArr(MyR_Id).Receive(SPacket => New_Packet);

      end;

      loop
         select

            accept Receive_Packet(P : Std_Packet) do

               My_P := P;

            end Receive_Packet;

            declare

               Msg : Unbounded_String;

            begin

               Msg := stdPacketToString(My_P);

               Msg := To_Unbounded_String("Host: ") & To_Unbounded_String(" (") & Integer'Image(MyR_Id) & To_Unbounded_String(" , ")
                 & Integer'Image(MyH_Id) & To_Unbounded_String(" )") & To_Unbounded_String(" ") & To_Unbounded_String("otrzymal pakiet: ")
                 & Msg;

               Server.Acc(Msg);

            end ;

            delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(RGen));

            S_Addr := Addr'(R => MyR_Id,
                           H => MyH_Id);

            R_Addr := Addr'(My_P.S.R, My_P.S.H);

            To_Send := Std_Packet'(S_Addr, R_Addr, Vr);

            RForwardersArr(MyR_Id).Receive(SPacket => To_Send);


         end select;

      end loop;


   end Host;

   task type Vertice is
       entry Init(Id : Integer ; N : Integer ; Neighbours : Integer_Vectors.Vector ; E_Hosts : Integer_Vectors.Vector);
   end Vertice;

   task body Vertice is
      MyId : Integer;
      MyN : Integer;
      MyNeighbours : Integer_Vectors.Vector;
      MySender : Sender;
      MyE_Hosts : Integer_Vectors.Vector;
      SF : Sending_Forwarder;
   begin

      accept Init (Id : in Integer; N : in Integer; Neighbours : in Integer_Vectors.Vector ; E_Hosts : Integer_Vectors.Vector) do
         MyId := Id;
         MyN := N;
         MyNeighbours := Neighbours;
         MyE_Hosts  := E_Hosts;
      end Init;

      declare
         Number_Of_Hosts : Integer;
      begin

         Number_Of_Hosts := MyE_Hosts(MyId);

         for I in 0..Number_Of_Hosts-1 loop

            HArr(MyId, I).Init(R_Id       => MyId,
                         H_Id       => I,
                         Neighbours => MyNeighbours,
                         E_Hosts    => MyE_Hosts);

         end loop;

      end;

      Prot_Arr(MyId).Init(MyN, MyId, MyNeighbours);

      MySender.Init(Id         => MyId,
                    N          => MyN,
                    Neighbours => MyNeighbours
                   );

      RArr(MyId).Init(MyId       => MyId,
                        N          => MyN,
                        Neighbours => MyNeighbours
                     );

      RForwardersArr(MyId).Init(MyId);
      SF.Init(MyId);

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
   Max_H : Integer;
   V1 : Integer;
   V2 : Integer;
   Len : Integer;
   NArr : ArrOfVector;
   IntGen : Generator;
   Existing_Hosts : Integer_Vectors.Vector;


begin

   Ada.Text_IO.Put("Wprowadz liczbe wierzcholkow w grafie: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(N);

   Ada.Text_IO.Put("Wprowadz liczbe skrotow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(D);

   Ada.Text_IO.Put("Podaj maksymalna liczbe hostow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(Max_H);

   Reset(IntGen);

   for I in 0..N-2 loop

      NArr(I).Append(I+1);
      NArr(I+1).Append(I);

   end loop;

   for I in 0..(N-1) loop

      V1 := (Integer(Random(IntGen)) mod Max_H) + 1;

      Existing_Hosts.Append(V1);

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
      Ada.Text_IO.Put("Wierzcholek: " & Integer'Image(I) & " | Liczba hostow: " & Integer'Image(Existing_Hosts(I)));
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
         A(I).Init(I, N, NArr(I), Existing_Hosts);
      end loop;

   end;

   loop
      null;
   end loop;

end Main;
