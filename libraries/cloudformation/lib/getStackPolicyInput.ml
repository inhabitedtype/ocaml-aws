open Aws.BaseTypes

type t = { stack_name : String.t }

let make ~stack_name () = { stack_name }

let parse xml =
  Some
    { stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("StackName", String.to_json v.stack_name) ])

let of_json j =
  { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName")) }
