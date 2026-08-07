open Aws.BaseTypes

type t = { resource_scan_id : String.t }

let make ~resource_scan_id () = { resource_scan_id }

let parse xml =
  Some
    { resource_scan_id =
        Aws.Xml.required
          "ResourceScanId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceScanId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ResourceScanId", String.to_query v.resource_scan_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ResourceScanId", String.to_json v.resource_scan_id) ])

let of_json j =
  { resource_scan_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceScanId"))
  }
