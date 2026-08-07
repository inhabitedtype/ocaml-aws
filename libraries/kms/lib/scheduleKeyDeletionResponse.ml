open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; deletion_date : DateTime.t option
  ; key_state : KeyState.t option
  ; pending_window_in_days : Integer.t option
  }

let make ?key_id ?deletion_date ?key_state ?pending_window_in_days () =
  { key_id; deletion_date; key_state; pending_window_in_days }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; deletion_date =
        Aws.Util.option_bind (Aws.Xml.member "DeletionDate" xml) DateTime.parse
    ; key_state = Aws.Util.option_bind (Aws.Xml.member "KeyState" xml) KeyState.parse
    ; pending_window_in_days =
        Aws.Util.option_bind (Aws.Xml.member "PendingWindowInDays" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pending_window_in_days (fun f ->
             Aws.Query.Pair ("PendingWindowInDays", Integer.to_query f))
       ; Aws.Util.option_map v.key_state (fun f ->
             Aws.Query.Pair ("KeyState", KeyState.to_query f))
       ; Aws.Util.option_map v.deletion_date (fun f ->
             Aws.Query.Pair ("DeletionDate", DateTime.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pending_window_in_days (fun f ->
             "PendingWindowInDays", Integer.to_json f)
       ; Aws.Util.option_map v.key_state (fun f -> "KeyState", KeyState.to_json f)
       ; Aws.Util.option_map v.deletion_date (fun f -> "DeletionDate", DateTime.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; deletion_date =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionDate") DateTime.of_json
  ; key_state = Aws.Util.option_map (Aws.Json.lookup j "KeyState") KeyState.of_json
  ; pending_window_in_days =
      Aws.Util.option_map (Aws.Json.lookup j "PendingWindowInDays") Integer.of_json
  }
