open Aws.BaseTypes

type t = { stack_set_id : String.t option }

let make ?stack_set_id () = { stack_set_id }

let parse xml =
  Some
    { stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set_id (fun f ->
             Aws.Query.Pair ("StackSetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f) ])

let of_json j =
  { stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json }
