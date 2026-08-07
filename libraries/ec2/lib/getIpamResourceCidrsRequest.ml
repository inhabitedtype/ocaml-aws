open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; ipam_scope_id : String.t
  ; ipam_pool_id : String.t option
  ; resource_id : String.t option
  ; resource_type : IpamResourceType.t option
  ; resource_tag : RequestIpamResourceTag.t option
  ; resource_owner : String.t option
  }

let make
    ?dry_run
    ?(filters = [])
    ?max_results
    ?next_token
    ~ipam_scope_id
    ?ipam_pool_id
    ?resource_id
    ?resource_type
    ?resource_tag
    ?resource_owner
    () =
  { dry_run
  ; filters
  ; max_results
  ; next_token
  ; ipam_scope_id
  ; ipam_pool_id
  ; resource_id
  ; resource_type
  ; resource_tag
  ; resource_owner
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; ipam_scope_id =
        Aws.Xml.required
          "IpamScopeId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamScopeId" xml) String.parse)
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) IpamResourceType.parse
    ; resource_tag =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceTag" xml)
          RequestIpamResourceTag.parse
    ; resource_owner =
        Aws.Util.option_bind (Aws.Xml.member "ResourceOwner" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_owner (fun f ->
             Aws.Query.Pair ("ResourceOwner", String.to_query f))
       ; Aws.Util.option_map v.resource_tag (fun f ->
             Aws.Query.Pair ("ResourceTag", RequestIpamResourceTag.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamScopeId", String.to_query v.ipam_scope_id))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_owner (fun f -> "ResourceOwner", String.to_json f)
       ; Aws.Util.option_map v.resource_tag (fun f ->
             "ResourceTag", RequestIpamResourceTag.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", IpamResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "IpamPoolId", String.to_json f)
       ; Some ("IpamScopeId", String.to_json v.ipam_scope_id)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; ipam_scope_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamScopeId"))
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "IpamPoolId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceType") IpamResourceType.of_json
  ; resource_tag =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceTag") RequestIpamResourceTag.of_json
  ; resource_owner =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceOwner") String.of_json
  }
