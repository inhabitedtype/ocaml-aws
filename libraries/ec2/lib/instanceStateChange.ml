open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; current_state : InstanceState.t option
  ; previous_state : InstanceState.t option
  }

let make ?instance_id ?current_state ?previous_state () =
  { instance_id; current_state; previous_state }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; current_state =
        Aws.Util.option_bind (Aws.Xml.member "currentState" xml) InstanceState.parse
    ; previous_state =
        Aws.Util.option_bind (Aws.Xml.member "previousState" xml) InstanceState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.previous_state (fun f ->
             Aws.Query.Pair ("PreviousState", InstanceState.to_query f))
       ; Aws.Util.option_map v.current_state (fun f ->
             Aws.Query.Pair ("CurrentState", InstanceState.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.previous_state (fun f ->
             "previousState", InstanceState.to_json f)
       ; Aws.Util.option_map v.current_state (fun f ->
             "currentState", InstanceState.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; current_state =
      Aws.Util.option_map (Aws.Json.lookup j "currentState") InstanceState.of_json
  ; previous_state =
      Aws.Util.option_map (Aws.Json.lookup j "previousState") InstanceState.of_json
  }
