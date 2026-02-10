module Value = Runtime.Sim.Value

(* Resubmit exception:

   Escape to end of ingress block, and prepare to set up metadata
   before feeding packet back into pipeline
   ctx_caller * sto * ctx * index *)

exception Resubmit of (Value.t * Value.t * Value.t * Value.t)
