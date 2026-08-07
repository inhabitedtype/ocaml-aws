open Aws.BaseTypes

type t =
  { min_capacity : Double.t option
  ; max_capacity : Double.t option
  ; seconds_until_auto_pause : Integer.t option
  }

let make ?min_capacity ?max_capacity ?seconds_until_auto_pause () =
  { min_capacity; max_capacity; seconds_until_auto_pause }

let parse xml =
  Some
    { min_capacity = Aws.Util.option_bind (Aws.Xml.member "MinCapacity" xml) Double.parse
    ; max_capacity = Aws.Util.option_bind (Aws.Xml.member "MaxCapacity" xml) Double.parse
    ; seconds_until_auto_pause =
        Aws.Util.option_bind (Aws.Xml.member "SecondsUntilAutoPause" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.seconds_until_auto_pause (fun f ->
             Aws.Query.Pair ("SecondsUntilAutoPause", Integer.to_query f))
       ; Aws.Util.option_map v.max_capacity (fun f ->
             Aws.Query.Pair ("MaxCapacity", Double.to_query f))
       ; Aws.Util.option_map v.min_capacity (fun f ->
             Aws.Query.Pair ("MinCapacity", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.seconds_until_auto_pause (fun f ->
             "SecondsUntilAutoPause", Integer.to_json f)
       ; Aws.Util.option_map v.max_capacity (fun f -> "MaxCapacity", Double.to_json f)
       ; Aws.Util.option_map v.min_capacity (fun f -> "MinCapacity", Double.to_json f)
       ])

let of_json j =
  { min_capacity = Aws.Util.option_map (Aws.Json.lookup j "MinCapacity") Double.of_json
  ; max_capacity = Aws.Util.option_map (Aws.Json.lookup j "MaxCapacity") Double.of_json
  ; seconds_until_auto_pause =
      Aws.Util.option_map (Aws.Json.lookup j "SecondsUntilAutoPause") Integer.of_json
  }
