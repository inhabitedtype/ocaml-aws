open Aws.BaseTypes

type t = { new_association_id : String.t option }

let make ?new_association_id () = { new_association_id }

let parse xml =
  Some
    { new_association_id =
        Aws.Util.option_bind (Aws.Xml.member "newAssociationId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.new_association_id (fun f ->
             Aws.Query.Pair ("NewAssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.new_association_id (fun f ->
             "newAssociationId", String.to_json f)
       ])

let of_json j =
  { new_association_id =
      Aws.Util.option_map (Aws.Json.lookup j "newAssociationId") String.of_json
  }
