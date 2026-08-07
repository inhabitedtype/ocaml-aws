open Aws.BaseTypes

type t = { location_name : String.t option }

let make ?location_name () = { location_name }

let parse xml =
  Some
    { location_name =
        Aws.Util.option_bind (Aws.Xml.member "LocationName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location_name (fun f ->
             Aws.Query.Pair ("LocationName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location_name (fun f -> "LocationName", String.to_json f) ])

let of_json j =
  { location_name = Aws.Util.option_map (Aws.Json.lookup j "LocationName") String.of_json
  }
