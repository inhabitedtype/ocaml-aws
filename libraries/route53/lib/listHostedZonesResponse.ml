open Aws.BaseTypes

type t =
  { hosted_zones : HostedZones.t
  ; marker : String.t option
  ; is_truncated : Boolean.t
  ; next_marker : String.t option
  ; max_items : String.t
  }

let make ~hosted_zones ?marker ~is_truncated ?next_marker ~max_items () =
  { hosted_zones; marker; is_truncated; next_marker; max_items }

let parse xml =
  Some
    { hosted_zones =
        Aws.Xml.required
          "HostedZones"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZones" xml) HostedZones.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Aws.Util.option_map v.next_marker (fun f ->
             Aws.Query.Pair ("NextMarker", String.to_query f))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("HostedZones", HostedZones.to_json v.hosted_zones)
       ])

let of_json j =
  { hosted_zones =
      HostedZones.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZones"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
