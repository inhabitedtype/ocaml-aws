open Aws.BaseTypes

type t = { hosted_zone_id : String.t }

let make ~hosted_zone_id () = { hosted_zone_id }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("Id", String.to_json v.hosted_zone_id) ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id")) }
