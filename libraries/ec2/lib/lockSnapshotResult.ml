open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; lock_state : LockState.t option
  ; lock_duration : Integer.t option
  ; cool_off_period : Integer.t option
  ; cool_off_period_expires_on : DateTime.t option
  ; lock_created_on : DateTime.t option
  ; lock_expires_on : DateTime.t option
  ; lock_duration_start_time : DateTime.t option
  }

let make
    ?snapshot_id
    ?lock_state
    ?lock_duration
    ?cool_off_period
    ?cool_off_period_expires_on
    ?lock_created_on
    ?lock_expires_on
    ?lock_duration_start_time
    () =
  { snapshot_id
  ; lock_state
  ; lock_duration
  ; cool_off_period
  ; cool_off_period_expires_on
  ; lock_created_on
  ; lock_expires_on
  ; lock_duration_start_time
  }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; lock_state = Aws.Util.option_bind (Aws.Xml.member "lockState" xml) LockState.parse
    ; lock_duration =
        Aws.Util.option_bind (Aws.Xml.member "lockDuration" xml) Integer.parse
    ; cool_off_period =
        Aws.Util.option_bind (Aws.Xml.member "coolOffPeriod" xml) Integer.parse
    ; cool_off_period_expires_on =
        Aws.Util.option_bind (Aws.Xml.member "coolOffPeriodExpiresOn" xml) DateTime.parse
    ; lock_created_on =
        Aws.Util.option_bind (Aws.Xml.member "lockCreatedOn" xml) DateTime.parse
    ; lock_expires_on =
        Aws.Util.option_bind (Aws.Xml.member "lockExpiresOn" xml) DateTime.parse
    ; lock_duration_start_time =
        Aws.Util.option_bind (Aws.Xml.member "lockDurationStartTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.lock_duration_start_time (fun f ->
             Aws.Query.Pair ("LockDurationStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.lock_expires_on (fun f ->
             Aws.Query.Pair ("LockExpiresOn", DateTime.to_query f))
       ; Aws.Util.option_map v.lock_created_on (fun f ->
             Aws.Query.Pair ("LockCreatedOn", DateTime.to_query f))
       ; Aws.Util.option_map v.cool_off_period_expires_on (fun f ->
             Aws.Query.Pair ("CoolOffPeriodExpiresOn", DateTime.to_query f))
       ; Aws.Util.option_map v.cool_off_period (fun f ->
             Aws.Query.Pair ("CoolOffPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.lock_duration (fun f ->
             Aws.Query.Pair ("LockDuration", Integer.to_query f))
       ; Aws.Util.option_map v.lock_state (fun f ->
             Aws.Query.Pair ("LockState", LockState.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.lock_duration_start_time (fun f ->
             "lockDurationStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.lock_expires_on (fun f ->
             "lockExpiresOn", DateTime.to_json f)
       ; Aws.Util.option_map v.lock_created_on (fun f ->
             "lockCreatedOn", DateTime.to_json f)
       ; Aws.Util.option_map v.cool_off_period_expires_on (fun f ->
             "coolOffPeriodExpiresOn", DateTime.to_json f)
       ; Aws.Util.option_map v.cool_off_period (fun f ->
             "coolOffPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.lock_duration (fun f -> "lockDuration", Integer.to_json f)
       ; Aws.Util.option_map v.lock_state (fun f -> "lockState", LockState.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; lock_state = Aws.Util.option_map (Aws.Json.lookup j "lockState") LockState.of_json
  ; lock_duration = Aws.Util.option_map (Aws.Json.lookup j "lockDuration") Integer.of_json
  ; cool_off_period =
      Aws.Util.option_map (Aws.Json.lookup j "coolOffPeriod") Integer.of_json
  ; cool_off_period_expires_on =
      Aws.Util.option_map (Aws.Json.lookup j "coolOffPeriodExpiresOn") DateTime.of_json
  ; lock_created_on =
      Aws.Util.option_map (Aws.Json.lookup j "lockCreatedOn") DateTime.of_json
  ; lock_expires_on =
      Aws.Util.option_map (Aws.Json.lookup j "lockExpiresOn") DateTime.of_json
  ; lock_duration_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "lockDurationStartTime") DateTime.of_json
  }
