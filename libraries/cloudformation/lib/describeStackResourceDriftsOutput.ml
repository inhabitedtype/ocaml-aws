open Aws.BaseTypes

type t =
  { stack_resource_drifts : StackResourceDrifts.t
  ; next_token : String.t option
  }

let make ~stack_resource_drifts ?next_token () = { stack_resource_drifts; next_token }

let parse xml =
  Some
    { stack_resource_drifts =
        Aws.Xml.required
          "StackResourceDrifts"
          (Aws.Util.option_bind
             (Aws.Xml.member "StackResourceDrifts" xml)
             StackResourceDrifts.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StackResourceDrifts.member"
              , StackResourceDrifts.to_query v.stack_resource_drifts ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("StackResourceDrifts", StackResourceDrifts.to_json v.stack_resource_drifts)
       ])

let of_json j =
  { stack_resource_drifts =
      StackResourceDrifts.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDrifts"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
