open Aws.BaseTypes

type t = { stack_refactor_id : String.t }

let make ~stack_refactor_id () = { stack_refactor_id }

let parse xml =
  Some
    { stack_refactor_id =
        Aws.Xml.required
          "StackRefactorId"
          (Aws.Util.option_bind (Aws.Xml.member "StackRefactorId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StackRefactorId", String.to_query v.stack_refactor_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackRefactorId", String.to_json v.stack_refactor_id) ])

let of_json j =
  { stack_refactor_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackRefactorId"))
  }
