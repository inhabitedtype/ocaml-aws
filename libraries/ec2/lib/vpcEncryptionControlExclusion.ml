open Aws.BaseTypes

type t =
  { state : VpcEncryptionControlExclusionState.t option
  ; state_message : String.t option
  }

let make ?state ?state_message () = { state; state_message }

let parse xml =
  Some
    { state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          VpcEncryptionControlExclusionState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpcEncryptionControlExclusionState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", VpcEncryptionControlExclusionState.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        VpcEncryptionControlExclusionState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  }
