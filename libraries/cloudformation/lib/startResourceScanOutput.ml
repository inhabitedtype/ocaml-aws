open Aws.BaseTypes

type t = { resource_scan_id : String.t option }

let make ?resource_scan_id () = { resource_scan_id }

let parse xml =
  Some
    { resource_scan_id =
        Aws.Util.option_bind (Aws.Xml.member "ResourceScanId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_scan_id (fun f ->
             Aws.Query.Pair ("ResourceScanId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_scan_id (fun f ->
             "ResourceScanId", String.to_json f)
       ])

let of_json j =
  { resource_scan_id =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceScanId") String.of_json
  }
