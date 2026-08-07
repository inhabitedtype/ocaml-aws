open Aws.BaseTypes

type t =
  { action_id : String.t
  ; status : String.t
  }

let make ~action_id ~status () = { action_id; status }

let parse xml =
  Some
    { action_id =
        Aws.Xml.required
          "ActionId"
          (Aws.Util.option_bind (Aws.Xml.member "ActionId" xml) String.parse)
    ; status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Status", String.to_query v.status))
       ; Some (Aws.Query.Pair ("ActionId", String.to_query v.action_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Status", String.to_json v.status)
       ; Some ("ActionId", String.to_json v.action_id)
       ])

let of_json j =
  { action_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActionId"))
  ; status = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  }
