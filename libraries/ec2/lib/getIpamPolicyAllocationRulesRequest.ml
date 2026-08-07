open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_policy_id : String.t
  ; filters : FilterList.t
  ; locale : String.t option
  ; resource_type : IpamPolicyResourceType.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?dry_run
    ~ipam_policy_id
    ?(filters = [])
    ?locale
    ?resource_type
    ?max_results
    ?next_token
    () =
  { dry_run; ipam_policy_id; filters; locale; resource_type; max_results; next_token }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_policy_id =
        Aws.Xml.required
          "IpamPolicyId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPolicyId" xml) String.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; locale = Aws.Util.option_bind (Aws.Xml.member "Locale" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceType" xml)
          IpamPolicyResourceType.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamPolicyResourceType.to_query f))
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some (Aws.Query.Pair ("IpamPolicyId", String.to_query v.ipam_policy_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", IpamPolicyResourceType.to_json f)
       ; Aws.Util.option_map v.locale (fun f -> "Locale", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some ("IpamPolicyId", String.to_json v.ipam_policy_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_policy_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPolicyId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "Locale") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceType")
        IpamPolicyResourceType.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
