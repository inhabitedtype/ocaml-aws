open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; max_results : Integer.t option
  ; dry_run : Boolean.t option
  ; network_acl_ids : NetworkAclIdStringList.t
  ; filters : FilterList.t
  }

let make ?next_token ?max_results ?dry_run ?(network_acl_ids = []) ?(filters = []) () =
  { next_token; max_results; dry_run; network_acl_ids; filters }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_acl_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkAclId" xml)
             NetworkAclIdStringList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ("NetworkAclId", NetworkAclIdStringList.to_query v.network_acl_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Some ("NetworkAclId", NetworkAclIdStringList.to_json v.network_acl_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_acl_ids =
      NetworkAclIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkAclId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
