open Aws.BaseTypes

type t =
  { key_id : String.t
  ; pending_window_in_days : Integer.t option
  }

let make ~key_id ?pending_window_in_days () = { key_id; pending_window_in_days }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; pending_window_in_days =
        Aws.Util.option_bind (Aws.Xml.member "PendingWindowInDays" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pending_window_in_days (fun f ->
             Aws.Query.Pair ("PendingWindowInDays", Integer.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pending_window_in_days (fun f ->
             "PendingWindowInDays", Integer.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; pending_window_in_days =
      Aws.Util.option_map (Aws.Json.lookup j "PendingWindowInDays") Integer.of_json
  }
