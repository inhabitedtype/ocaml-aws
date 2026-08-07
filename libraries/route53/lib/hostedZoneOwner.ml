open Aws.BaseTypes

type t =
  { owning_account : String.t option
  ; owning_service : String.t option
  }

let make ?owning_account ?owning_service () = { owning_account; owning_service }

let parse xml =
  Some
    { owning_account =
        Aws.Util.option_bind (Aws.Xml.member "OwningAccount" xml) String.parse
    ; owning_service =
        Aws.Util.option_bind (Aws.Xml.member "OwningService" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owning_service (fun f ->
             Aws.Query.Pair ("OwningService", String.to_query f))
       ; Aws.Util.option_map v.owning_account (fun f ->
             Aws.Query.Pair ("OwningAccount", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owning_service (fun f -> "OwningService", String.to_json f)
       ; Aws.Util.option_map v.owning_account (fun f -> "OwningAccount", String.to_json f)
       ])

let of_json j =
  { owning_account =
      Aws.Util.option_map (Aws.Json.lookup j "OwningAccount") String.of_json
  ; owning_service =
      Aws.Util.option_map (Aws.Json.lookup j "OwningService") String.of_json
  }
