open Aws.BaseTypes

type t =
  { flow_logs : FlowLogSet.t
  ; next_token : String.t option
  }

let make ?(flow_logs = []) ?next_token () = { flow_logs; next_token }

let parse xml =
  Some
    { flow_logs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "flowLogSet" xml) FlowLogSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("FlowLogSet", FlowLogSet.to_query v.flow_logs))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("flowLogSet", FlowLogSet.to_json v.flow_logs)
       ])

let of_json j =
  { flow_logs =
      FlowLogSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "flowLogSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
