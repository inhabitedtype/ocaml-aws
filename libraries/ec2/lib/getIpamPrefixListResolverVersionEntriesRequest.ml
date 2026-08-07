open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_prefix_list_resolver_id : String.t
  ; ipam_prefix_list_resolver_version : Long.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?dry_run
    ~ipam_prefix_list_resolver_id
    ~ipam_prefix_list_resolver_version
    ?max_results
    ?next_token
    () =
  { dry_run
  ; ipam_prefix_list_resolver_id
  ; ipam_prefix_list_resolver_version
  ; max_results
  ; next_token
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_prefix_list_resolver_id =
        Aws.Xml.required
          "IpamPrefixListResolverId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamPrefixListResolverId" xml)
             String.parse)
    ; ipam_prefix_list_resolver_version =
        Aws.Xml.required
          "IpamPrefixListResolverVersion"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamPrefixListResolverVersion" xml)
             Long.parse)
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
       ; Some
           (Aws.Query.Pair
              ( "IpamPrefixListResolverVersion"
              , Long.to_query v.ipam_prefix_list_resolver_version ))
       ; Some
           (Aws.Query.Pair
              ("IpamPrefixListResolverId", String.to_query v.ipam_prefix_list_resolver_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "IpamPrefixListResolverVersion"
           , Long.to_json v.ipam_prefix_list_resolver_version )
       ; Some ("IpamPrefixListResolverId", String.to_json v.ipam_prefix_list_resolver_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_prefix_list_resolver_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPrefixListResolverId"))
  ; ipam_prefix_list_resolver_version =
      Long.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPrefixListResolverVersion"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
