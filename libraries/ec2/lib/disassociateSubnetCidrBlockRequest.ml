open Aws.BaseTypes

type t = { association_id : String.t }

let make ~association_id () = { association_id }

let parse xml =
  Some
    { association_id =
        Aws.Xml.required
          "associationId"
          (Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AssociationId", String.to_query v.association_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("associationId", String.to_json v.association_id) ])

let of_json j =
  { association_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "associationId"))
  }
