

--
-- Generic protected queue package
--


generic
   type Data_Type is private;

package PQueue is

   type Queue_Type is limited private;

   protected type Protected_Queue_Type is
      procedure Enqueue (Item:  in  Data_Type);
      entry     Dequeue (Item:  out Data_Type);
      function  Length return natural;
   private
      Queue:  Queue_Type;
   end Protected_Queue_Type;

private
   type Queue_Element_Type;
   type Queue_Element_Pointer is access Queue_Element_Type;
   type Queue_Element_Type is record
      Data:  Data_Type;
      Next:  Queue_Element_Pointer;
   end record;

   type Queue_Type is record
      Len:    natural := 0;
      Front:  Queue_Element_Pointer := null;
      Back:   Queue_Element_Pointer := null;
   end record;

end PQueue;
