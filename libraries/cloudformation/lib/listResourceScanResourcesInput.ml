open Aws.BaseTypes

type t =
  { resource_scan_id : String.t
  ; resource_identifier : String.t option
  ; resource_type_prefix : String.t option
  ; tag_key : String.t option
  ; tag_value : String.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ~resource_scan_id
    ?resource_identifier
    ?resource_type_prefix
    ?tag_key
    ?tag_value
    ?next_token
    ?max_results
    () =
  { resource_scan_id
  ; resource_identifier
  ; resource_type_prefix
  ; tag_key
  ; tag_value
  ; next_token
  ; max_results
  }

let parse xml =
  Some
    { resource_scan_id =
        Aws.Xml.required
          "ResourceScanId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceScanId" xml) String.parse)
    ; resource_identifier =
        Aws.Util.option_bind (Aws.Xml.member "ResourceIdentifier" xml) String.parse
    ; resource_type_prefix =
        Aws.Util.option_bind (Aws.Xml.member "ResourceTypePrefix" xml) String.parse
    ; tag_key = Aws.Util.option_bind (Aws.Xml.member "TagKey" xml) String.parse
    ; tag_value = Aws.Util.option_bind (Aws.Xml.member "TagValue" xml) String.parse
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
       ; Aws.Util.option_map v.tag_value (fun f ->
             Aws.Query.Pair ("TagValue", String.to_query f))
       ; Aws.Util.option_map v.tag_key (fun f ->
             Aws.Query.Pair ("TagKey", String.to_query f))
       ; Aws.Util.option_map v.resource_type_prefix (fun f ->
             Aws.Query.Pair ("ResourceTypePrefix", String.to_query f))
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             Aws.Query.Pair ("ResourceIdentifier", String.to_query f))
       ; Some (Aws.Query.Pair ("ResourceScanId", String.to_query v.resource_scan_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.tag_value (fun f -> "TagValue", String.to_json f)
       ; Aws.Util.option_map v.tag_key (fun f -> "TagKey", String.to_json f)
       ; Aws.Util.option_map v.resource_type_prefix (fun f ->
             "ResourceTypePrefix", String.to_json f)
       ; Aws.Util.option_map v.resource_identifier (fun f ->
             "ResourceIdentifier", String.to_json f)
       ; Some ("ResourceScanId", String.to_json v.resource_scan_id)
       ])

let of_json j =
  { resource_scan_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceScanId"))
  ; resource_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceIdentifier") String.of_json
  ; resource_type_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceTypePrefix") String.of_json
  ; tag_key = Aws.Util.option_map (Aws.Json.lookup j "TagKey") String.of_json
  ; tag_value = Aws.Util.option_map (Aws.Json.lookup j "TagValue") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
