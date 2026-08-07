open Aws.BaseTypes

type t =
  { snapshot_id : String.t
  ; dry_run : Boolean.t option
  ; lock_mode : LockMode.t
  ; cool_off_period : Integer.t option
  ; lock_duration : Integer.t option
  ; expiration_date : DateTime.t option
  }

let make
    ~snapshot_id
    ?dry_run
    ~lock_mode
    ?cool_off_period
    ?lock_duration
    ?expiration_date
    () =
  { snapshot_id; dry_run; lock_mode; cool_off_period; lock_duration; expiration_date }

let parse xml =
  Some
    { snapshot_id =
        Aws.Xml.required
          "SnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; lock_mode =
        Aws.Xml.required
          "LockMode"
          (Aws.Util.option_bind (Aws.Xml.member "LockMode" xml) LockMode.parse)
    ; cool_off_period =
        Aws.Util.option_bind (Aws.Xml.member "CoolOffPeriod" xml) Integer.parse
    ; lock_duration =
        Aws.Util.option_bind (Aws.Xml.member "LockDuration" xml) Integer.parse
    ; expiration_date =
        Aws.Util.option_bind (Aws.Xml.member "ExpirationDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expiration_date (fun f ->
             Aws.Query.Pair ("ExpirationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.lock_duration (fun f ->
             Aws.Query.Pair ("LockDuration", Integer.to_query f))
       ; Aws.Util.option_map v.cool_off_period (fun f ->
             Aws.Query.Pair ("CoolOffPeriod", Integer.to_query f))
       ; Some (Aws.Query.Pair ("LockMode", LockMode.to_query v.lock_mode))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expiration_date (fun f ->
             "ExpirationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.lock_duration (fun f -> "LockDuration", Integer.to_json f)
       ; Aws.Util.option_map v.cool_off_period (fun f ->
             "CoolOffPeriod", Integer.to_json f)
       ; Some ("LockMode", LockMode.to_json v.lock_mode)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("SnapshotId", String.to_json v.snapshot_id)
       ])

let of_json j =
  { snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; lock_mode = LockMode.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LockMode"))
  ; cool_off_period =
      Aws.Util.option_map (Aws.Json.lookup j "CoolOffPeriod") Integer.of_json
  ; lock_duration = Aws.Util.option_map (Aws.Json.lookup j "LockDuration") Integer.of_json
  ; expiration_date =
      Aws.Util.option_map (Aws.Json.lookup j "ExpirationDate") DateTime.of_json
  }
