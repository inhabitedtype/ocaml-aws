open Aws.BaseTypes

type t =
  { encryption_state : EncryptionStateValue.t option
  ; state_message : String.t option
  }

let make ?encryption_state ?state_message () = { encryption_state; state_message }

let parse xml =
  Some
    { encryption_state =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionState" xml)
          EncryptionStateValue.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.encryption_state (fun f ->
             Aws.Query.Pair ("EncryptionState", EncryptionStateValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.encryption_state (fun f ->
             "encryptionState", EncryptionStateValue.to_json f)
       ])

let of_json j =
  { encryption_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionState")
        EncryptionStateValue.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  }
