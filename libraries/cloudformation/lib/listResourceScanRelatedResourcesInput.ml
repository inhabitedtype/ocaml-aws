open Aws.BaseTypes

type t =
  { resource_scan_id : String.t
  ; resources : ScannedResourceIdentifiers.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ~resource_scan_id ~resources ?next_token ?max_results () =
  { resource_scan_id; resources; next_token; max_results }

let parse xml =
  Some
    { resource_scan_id =
        Aws.Xml.required
          "ResourceScanId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceScanId" xml) String.parse)
    ; resources =
        Aws.Xml.required
          "Resources"
          (Aws.Util.option_bind
             (Aws.Xml.member "Resources" xml)
             ScannedResourceIdentifiers.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Resources.member", ScannedResourceIdentifiers.to_query v.resources))
       ; Some (Aws.Query.Pair ("ResourceScanId", String.to_query v.resource_scan_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Resources", ScannedResourceIdentifiers.to_json v.resources)
       ; Some ("ResourceScanId", String.to_json v.resource_scan_id)
       ])

let of_json j =
  { resource_scan_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceScanId"))
  ; resources =
      ScannedResourceIdentifiers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Resources"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
