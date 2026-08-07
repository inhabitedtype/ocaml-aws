open Aws.BaseTypes

type t =
  { stack_name : String.t option
  ; next_token : String.t option
  }

let make ?stack_name ?next_token () = { stack_name; next_token }

let parse xml =
  Some
    { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.stack_name (fun f ->
             Aws.Query.Pair ("StackName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
       ])

let of_json j =
  { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
