open Aws.BaseTypes

type t =
  { max_results : Integer.t option
  ; next_token : String.t option
  ; owner_ids : OwnerStringList.t
  ; restorable_by_user_ids : RestorableByStringList.t
  ; snapshot_ids : SnapshotIdStringList.t
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?max_results
    ?next_token
    ?(owner_ids = [])
    ?(restorable_by_user_ids = [])
    ?(snapshot_ids = [])
    ?dry_run
    ?(filters = [])
    () =
  { max_results
  ; next_token
  ; owner_ids
  ; restorable_by_user_ids
  ; snapshot_ids
  ; dry_run
  ; filters
  }

let parse xml =
  Some
    { max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; owner_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) OwnerStringList.parse)
    ; restorable_by_user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RestorableBy" xml)
             RestorableByStringList.parse)
    ; snapshot_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SnapshotId" xml)
             SnapshotIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("SnapshotId", SnapshotIdStringList.to_query v.snapshot_ids))
       ; Some
           (Aws.Query.Pair
              ("RestorableBy", RestorableByStringList.to_query v.restorable_by_user_ids))
       ; Some (Aws.Query.Pair ("Owner", OwnerStringList.to_query v.owner_ids))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("SnapshotId", SnapshotIdStringList.to_json v.snapshot_ids)
       ; Some ("RestorableBy", RestorableByStringList.to_json v.restorable_by_user_ids)
       ; Some ("Owner", OwnerStringList.to_json v.owner_ids)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ])

let of_json j =
  { max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; owner_ids =
      OwnerStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner"))
  ; restorable_by_user_ids =
      RestorableByStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RestorableBy"))
  ; snapshot_ids =
      SnapshotIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
