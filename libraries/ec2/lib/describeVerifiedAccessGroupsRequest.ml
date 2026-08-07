open Aws.BaseTypes

type t =
  { verified_access_group_ids : VerifiedAccessGroupIdList.t
  ; verified_access_instance_id : String.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; filters : FilterList.t
  ; dry_run : Boolean.t option
  }

let make
    ?(verified_access_group_ids = [])
    ?verified_access_instance_id
    ?max_results
    ?next_token
    ?(filters = [])
    ?dry_run
    () =
  { verified_access_group_ids
  ; verified_access_instance_id
  ; max_results
  ; next_token
  ; filters
  ; dry_run
  }

let parse xml =
  Some
    { verified_access_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessGroupId" xml)
             VerifiedAccessGroupIdList.parse)
    ; verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "VerifiedAccessInstanceId" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VerifiedAccessGroupId"
              , VerifiedAccessGroupIdList.to_query v.verified_access_group_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "VerifiedAccessInstanceId", String.to_json f)
       ; Some
           ( "VerifiedAccessGroupId"
           , VerifiedAccessGroupIdList.to_json v.verified_access_group_ids )
       ])

let of_json j =
  { verified_access_group_ids =
      VerifiedAccessGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessGroupId"))
  ; verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "VerifiedAccessInstanceId") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
