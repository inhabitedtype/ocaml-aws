open Aws.BaseTypes

type t = { id : String.t }

let make ~id () = { id }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "CidrCollectionId"
          (Aws.Util.option_bind (Aws.Xml.member "CidrCollectionId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CidrCollectionId", String.to_query v.id)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("CidrCollectionId", String.to_json v.id) ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CidrCollectionId")) }
