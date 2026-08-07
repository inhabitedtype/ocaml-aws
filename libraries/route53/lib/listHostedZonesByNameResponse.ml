open Aws.BaseTypes

type t =
  { hosted_zones : HostedZones.t
  ; d_n_s_name : String.t option
  ; hosted_zone_id : String.t option
  ; is_truncated : Boolean.t
  ; next_d_n_s_name : String.t option
  ; next_hosted_zone_id : String.t option
  ; max_items : String.t
  }

let make
    ~hosted_zones
    ?d_n_s_name
    ?hosted_zone_id
    ~is_truncated
    ?next_d_n_s_name
    ?next_hosted_zone_id
    ~max_items
    () =
  { hosted_zones
  ; d_n_s_name
  ; hosted_zone_id
  ; is_truncated
  ; next_d_n_s_name
  ; next_hosted_zone_id
  ; max_items
  }

let parse xml =
  Some
    { hosted_zones =
        Aws.Xml.required
          "HostedZones"
          (Aws.Util.option_bind (Aws.Xml.member "HostedZones" xml) HostedZones.parse)
    ; d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; next_d_n_s_name =
        Aws.Util.option_bind (Aws.Xml.member "NextDNSName" xml) String.parse
    ; next_hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "NextHostedZoneId" xml) String.parse
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Aws.Util.option_map v.next_hosted_zone_id (fun f ->
             Aws.Query.Pair ("NextHostedZoneId", String.to_query f))
       ; Aws.Util.option_map v.next_d_n_s_name (fun f ->
             Aws.Query.Pair ("NextDNSName", String.to_query f))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("HostedZoneId", String.to_query f))
       ; Aws.Util.option_map v.d_n_s_name (fun f ->
             Aws.Query.Pair ("DNSName", String.to_query f))
       ; Some (Aws.Query.Pair ("HostedZones.member", HostedZones.to_query v.hosted_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Aws.Util.option_map v.next_hosted_zone_id (fun f ->
             "NextHostedZoneId", String.to_json f)
       ; Aws.Util.option_map v.next_d_n_s_name (fun f -> "NextDNSName", String.to_json f)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Aws.Util.option_map v.hosted_zone_id (fun f -> "HostedZoneId", String.to_json f)
       ; Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f)
       ; Some ("HostedZones", HostedZones.to_json v.hosted_zones)
       ])

let of_json j =
  { hosted_zones =
      HostedZones.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HostedZones"))
  ; d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; next_d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "NextDNSName") String.of_json
  ; next_hosted_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "NextHostedZoneId") String.of_json
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
