with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

use Ada.Containers;

procedure z1l1b is

   type String is array (0..1) of Character;

   type randRange is new Integer range 0..10000;
   package Rand_Int is new Ada.Numerics.Discrete_Random(randRange);

   use Rand_Int;

   package Integer_Vectors is new Vectors(Natural, Integer);

   subtype Idx is Integer range 0..100;

   type ArrOfVector is array (Idx) of Integer_Vectors.Vector;

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


   N : Integer;
   D : Integer;
   K : Integer;
   AdjList : ArrOfVector;
   ServicedPackets : ArrOfVector;
   VisitedVertices : ArrOfVector;
   gen : Generator;
   gen2 : Generator;
   V1 : Integer;
   V2 : Integer;
   Len : Integer;
   Bound : Integer := 1;

  task Source is
      entry Init(Id : Integer ; MyNeighbors : Integer_Vectors.Vector);
      entry Produce(K : Integer);
  end Source;

   task Server is
      entry Send(Packet : Integer ; Vertice : Integer ; Message : String);
   end Server;


   task type Forwarder is
      entry Init(Id : Integer ; N : Integer ; MyNeighbors : Integer_Vectors.Vector);
      entry Pass(Packet : Integer);
   end Forwarder;

   type ArrOfTasks is array (Idx) of Forwarder;
   
   Forwarder_Arr : ArrOfTasks;

   task body Forwarder is
      Num : Integer;
      MyId : Integer;
      Next : Integer;
      MyPacket : Integer;
      Neighbors : Integer_Vectors.Vector;
      MyGen : Ada.Numerics.Float_Random.Generator;
      MyIntegerGen : Generator;
   begin
      loop
         select
            accept Init(Id : Integer ; N : Integer ; MyNeighbors : Integer_Vectors.Vector) do
               MyId := Id;
               Num := N;
               
               for I in MyNeighbors.First_Index..MyNeighbors.Last_Index loop
                  Neighbors.Append(MyNeighbors(I));
               end loop;
            end Init;
            or
            accept Pass(Packet : Integer) do
               if MyId = Num - 1 then
                  Ada.Numerics.Float_Random.reset(MyGen);
                  Server.Send(Packet, MyId, "RE");
                  delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(MyGen));
               else
                  Ada.Numerics.Float_Random.reset(MyGen);
                  reset(MyIntegerGen);
		  MyPacket = Packet;
                  Server.Send(Packet, MyId, "IN");
                  delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(MyGen));
                  Next := Integer(random(MyIntegerGen)) mod (Integer(Neighbors.Length));
                  Forwarder_Arr(Neighbors(Next)).Pass(Packet);
               end if;
            end Pass;
            or
            terminate;
         end select;
      end loop;
   end Forwarder;
   
   task body Source is
      MyId : Integer;
      Next : Integer;
      Neighbors : Integer_Vectors.Vector;
      MyGen : Ada.Numerics.Float_Random.Generator;
      MyIntegerGen : Generator;
   begin
      loop
         select
            accept Init(Id : Integer ; MyNeighbors : Integer_Vectors.Vector) do
                  MyId := Id;
                  
                  for I in MyNeighbors.First_Index..MyNeighbors.Last_Index loop
                     Neighbors.Append(MyNeighbors(I));
                  end loop;
                  
            end Init;
            or
            accept Produce(K : Integer) do
                            
               for I in 0..(K-1) loop
                  Ada.Numerics.Float_Random.reset(MyGen);
                  reset(MyIntegerGen);
                  delay Duration(Float(Bound)*Ada.Numerics.Float_Random.Random(MyGen));
                  Next := Integer(random(MyIntegerGen)) mod (Integer(Neighbors.Length));
                  Server.Send(I, 0, "IN");
                  Forwarder_Arr(Neighbors(Next)).Pass(I);
               end loop;
            end Produce;
            or
            terminate;
         end select;
      end loop;
   end Source;
                     

   task body Server is
   begin
      loop
         select
            accept Send(Packet : Integer ; Vertice : Integer ; Message : String) do

               if Message = "IN" then
                  ServicedPackets(Vertice).Append(Packet);
                  VisitedVertices(Packet).Append(Vertice);
                  Ada.Text_IO.Put("Pakiet " & Integer'Image(Packet) & " jest w wierzcholku " & Integer'Image(Vertice));
                  Ada.Text_IO.New_Line;
                          
               else
                  ServicedPackets(Vertice).Append(Packet);
                  VisitedVertices(Packet).Append(Vertice);
                  Ada.Text_IO.Put("Odebralem pakiet " & Integer'Image(Packet));
                  Ada.Text_IO.New_Line;
               end if;
            end Send;
            or
            terminate;
          end select;
       end loop;
   end Server;



begin

   Ada.Text_IO.Put("Wprowadz liczbe wierzcholkow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(N);

   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put("Wprowadz liczbe skrotow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(D);

   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put("Wprowadz liczbe pakietow: ");
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Get(K);
   
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put("Generuje graf...");
   Ada.Text_IO.New_Line;

   for I in 0..(N-2) loop

      AdjList(I).Append(I+1);

   end loop;


   for I in 0..(D-1) loop

      reset(gen);
      while True loop

         V1 := Integer(random(gen)) mod (N-1);
         V2 := Integer(random(gen)) mod N;

         exit when V1 < V2 and Search(AdjList(V1), V2) = False;
      end loop;

      AdjList(V1).Append(V2);

   end loop;

   Ada.Text_IO.Put("Graf wygenerowany. Drukuje...");
   Ada.Text_IO.New_Line;

   for I in 0..(N-1) loop
      Ada.Text_IO.Put("Wierzcholek: " & Integer'Image(I));
      Len := Integer(AdjList(I).Length);
      Ada.Text_IO.Put(" | Sasiedzi: ");
      for J in 0..(Len-1) loop

         Ada.Integer_Text_IO.Put(AdjList(I)(J));

      end loop;

      Ada.Text_IO.New_Line;

   end loop;

   Ada.Text_IO.Put("Rozpoczynam symulacje...");
   Ada.Text_IO.New_Line;
   
   for I in 1..N-1 loop
      Forwarder_Arr(I).Init(I, N, AdjList(I));
   end loop;
   
   Source.Init(0, AdjList(0));
   Source.Produce(K);
   
   Ada.Text_IO.Put("Symulacja zakonczona. Drukuje raporty koncowe...");
   Ada.Text_IO.New_Line(2);
   
   for I in 0..N-1 loop
      Ada.Text_IO.Put("Wierzcholek : " & Integer'Image(I) & " Obsluzone pakiety: ");
      Len := Integer(ServicedPackets(I).Length);
      
      for J in 0..(Len-1) loop
         
         Ada.Integer_Text_IO.Put(ServicedPackets(I)(J));
      end loop;
      
      Ada.Text_IO.New_Line;
      
   end loop;
   
   Ada.Text_IO.New_Line(2);
   
   
   for I in 0..K-1 loop
      Ada.Text_IO.Put("Pakiet : " & Integer'Image(I) & " Odwiedzone wierzcholki: ");
      Len := Integer(VisitedVertices(I).Length);
      
      for J in 0..(Len-1) loop
         Ada.Integer_Text_IO.Put(VisitedVertices(I)(J));
      end loop;
      
      Ada.Text_IO.New_Line;
      
   end loop;
        
     
end z1l1b;
