open Aws.BaseTypes

type t =
  { operation_events : OperationEvents.t
  ; next_token : String.t option
  }

let make ?(operation_events = []) ?next_token () = { operation_events; next_token }

let parse xml =
  Some
    { operation_events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OperationEvents" xml)
             OperationEvents.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OperationEvents.member", OperationEvents.to_query v.operation_events))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("OperationEvents", OperationEvents.to_json v.operation_events)
       ])

let of_json j =
  { operation_events =
      OperationEvents.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationEvents"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
