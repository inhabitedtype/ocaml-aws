type t =
  { state : SnapshotBlockPublicAccessState.t option
  ; managed_by : ManagedBy.t option
  }

let make ?state ?managed_by () = { state; managed_by }

let parse xml =
  Some
    { state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          SnapshotBlockPublicAccessState.parse
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SnapshotBlockPublicAccessState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", SnapshotBlockPublicAccessState.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        SnapshotBlockPublicAccessState.of_json
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  }
