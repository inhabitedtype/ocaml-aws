open Aws.BaseTypes

type t =
  { min_capacity : Integer.t option
  ; max_capacity : Integer.t option
  ; auto_pause : Boolean.t option
  ; seconds_until_auto_pause : Integer.t option
  ; timeout_action : String.t option
  ; seconds_before_timeout : Integer.t option
  }

let make
    ?min_capacity
    ?max_capacity
    ?auto_pause
    ?seconds_until_auto_pause
    ?timeout_action
    ?seconds_before_timeout
    () =
  { min_capacity
  ; max_capacity
  ; auto_pause
  ; seconds_until_auto_pause
  ; timeout_action
  ; seconds_before_timeout
  }

let parse xml =
  Some
    { min_capacity = Aws.Util.option_bind (Aws.Xml.member "MinCapacity" xml) Integer.parse
    ; max_capacity = Aws.Util.option_bind (Aws.Xml.member "MaxCapacity" xml) Integer.parse
    ; auto_pause = Aws.Util.option_bind (Aws.Xml.member "AutoPause" xml) Boolean.parse
    ; seconds_until_auto_pause =
        Aws.Util.option_bind (Aws.Xml.member "SecondsUntilAutoPause" xml) Integer.parse
    ; timeout_action =
        Aws.Util.option_bind (Aws.Xml.member "TimeoutAction" xml) String.parse
    ; seconds_before_timeout =
        Aws.Util.option_bind (Aws.Xml.member "SecondsBeforeTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.seconds_before_timeout (fun f ->
             Aws.Query.Pair ("SecondsBeforeTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.timeout_action (fun f ->
             Aws.Query.Pair ("TimeoutAction", String.to_query f))
       ; Aws.Util.option_map v.seconds_until_auto_pause (fun f ->
             Aws.Query.Pair ("SecondsUntilAutoPause", Integer.to_query f))
       ; Aws.Util.option_map v.auto_pause (fun f ->
             Aws.Query.Pair ("AutoPause", Boolean.to_query f))
       ; Aws.Util.option_map v.max_capacity (fun f ->
             Aws.Query.Pair ("MaxCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.min_capacity (fun f ->
             Aws.Query.Pair ("MinCapacity", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.seconds_before_timeout (fun f ->
             "SecondsBeforeTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.timeout_action (fun f -> "TimeoutAction", String.to_json f)
       ; Aws.Util.option_map v.seconds_until_auto_pause (fun f ->
             "SecondsUntilAutoPause", Integer.to_json f)
       ; Aws.Util.option_map v.auto_pause (fun f -> "AutoPause", Boolean.to_json f)
       ; Aws.Util.option_map v.max_capacity (fun f -> "MaxCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.min_capacity (fun f -> "MinCapacity", Integer.to_json f)
       ])

let of_json j =
  { min_capacity = Aws.Util.option_map (Aws.Json.lookup j "MinCapacity") Integer.of_json
  ; max_capacity = Aws.Util.option_map (Aws.Json.lookup j "MaxCapacity") Integer.of_json
  ; auto_pause = Aws.Util.option_map (Aws.Json.lookup j "AutoPause") Boolean.of_json
  ; seconds_until_auto_pause =
      Aws.Util.option_map (Aws.Json.lookup j "SecondsUntilAutoPause") Integer.of_json
  ; timeout_action =
      Aws.Util.option_map (Aws.Json.lookup j "TimeoutAction") String.of_json
  ; seconds_before_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "SecondsBeforeTimeout") Integer.of_json
  }
