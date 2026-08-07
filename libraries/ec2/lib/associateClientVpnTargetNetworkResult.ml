open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; status : AssociationStatus.t option
  }

let make ?association_id ?status () = { association_id; status }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AssociationStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AssociationStatus.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", AssociationStatus.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AssociationStatus.of_json
  }
