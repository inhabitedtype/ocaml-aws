open Aws.BaseTypes

type t =
  { stack_events : StackEvents.t
  ; next_token : String.t option
  }

let make ?(stack_events = []) ?next_token () = { stack_events; next_token }

let parse xml =
  Some
    { stack_events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StackEvents" xml) StackEvents.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("StackEvents.member", StackEvents.to_query v.stack_events))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackEvents", StackEvents.to_json v.stack_events)
       ])

let of_json j =
  { stack_events =
      StackEvents.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackEvents"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
