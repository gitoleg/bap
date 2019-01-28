let printf = Printf.printf
let () = printf "DEBUG: Bap_trace_std\n"
include Bap_trace_event_types
include Bap_trace_meta_types
module Event = Bap_trace_events
let () = printf "DEBUG: Bap_trace_std: just middle point\n%!"
module Meta = Bap_trace_meta
module Trace = Bap_trace
type trace = Trace.t
module Traces = Bap_trace_traces
let () = printf "DEBUG: register binprot tools\n%!"
let () = Bap_trace_binprot.register ()
let () = printf "DEBUG: registerED binprot tools\n%!"
