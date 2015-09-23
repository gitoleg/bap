(* Implementation notes

   Basically, there should be two implementations, one version is
   eager other is lazy. Once [memoize] function is called on should
   switch to an eager version.

   A simple approach would look like this:

   {[
     type events =
       | Stream of reader
       | Loaded of event array


     type t = {
       id : Id.t;
       tool : tool;
       meta : dict;
       events : events;
     }
   ]}

   Another approach would be to implement eager reader, that will take
   other reader and memoize it.
*)
