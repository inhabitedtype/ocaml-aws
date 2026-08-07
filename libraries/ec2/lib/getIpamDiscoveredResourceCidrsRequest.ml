open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_resource_discovery_id : String.t
  ; resource_region : String.t
  ; filters : FilterList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?dry_run
    ~ipam_resource_discovery_id
    ~resource_region
    ?(filters = [])
    ?next_token
    ?max_results
    () =
  { dry_run
  ; ipam_resource_discovery_id
  ; resource_region
  ; filters
  ; next_token
  ; max_results
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_resource_discovery_id =
        Aws.Xml.required
          "IpamResourceDiscoveryId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamResourceDiscoveryId" xml)
             String.parse)
    ; resource_region =
        Aws.Xml.required
          "ResourceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceRegion" xml) String.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
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
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some (Aws.Query.Pair ("ResourceRegion", String.to_query v.resource_region))
       ; Some
           (Aws.Query.Pair
              ("IpamResourceDiscoveryId", String.to_query v.ipam_resource_discovery_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some ("ResourceRegion", String.to_json v.resource_region)
       ; Some ("IpamResourceDiscoveryId", String.to_json v.ipam_resource_discovery_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_resource_discovery_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamResourceDiscoveryId"))
  ; resource_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRegion"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
