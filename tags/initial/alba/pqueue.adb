
--
-- Generic protected queues, body
--

with Ada.Unchecked_Deallocation;

package body PQueue is

   procedure Free_Queue_Element is new Ada.Unchecked_Deallocation (Queue_Element_Type,
                                                                   Queue_Element_Pointer);

   protected body Protected_Queue_Type is

      procedure Enqueue (Item:  in  Data_Type) is
         New_Element:  Queue_Element_Pointer;
      begin  -- Enqueue
         New_Element := new Queue_Element_Type'(Item, null);
         if Queue.Len = 0 then
            Queue.Front := New_Element;
         else
            Queue.Back.Next := New_Element;
         end if;
         Queue.Back := New_Element;
         Queue.Len := Queue.Len + 1;
      end Enqueue;

      entry     Dequeue (Item:  out Data_Type) when Queue.Len > 0 is
         Old_Front:  Queue_Element_Pointer;
      begin  -- Dequeue
         Old_Front := Queue.Front;
         Queue.Front := Queue.Front.Next;
         Item := Old_Front.Data;
         Free_Queue_Element (Old_Front);
         Queue.Len := Queue.Len - 1;
      end Dequeue;

      function  Length return Natural is
      begin  -- Length
         return Queue.Len;
      end Length;

   end Protected_Queue_Type;

end PQueue;
