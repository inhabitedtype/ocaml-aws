open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; mac_valid : Boolean.t option
  ; mac_algorithm : MacAlgorithmSpec.t option
  }

let make ?key_id ?mac_valid ?mac_algorithm () = { key_id; mac_valid; mac_algorithm }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; mac_valid = Aws.Util.option_bind (Aws.Xml.member "MacValid" xml) Boolean.parse
    ; mac_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "MacAlgorithm" xml) MacAlgorithmSpec.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mac_algorithm (fun f ->
             Aws.Query.Pair ("MacAlgorithm", MacAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.mac_valid (fun f ->
             Aws.Query.Pair ("MacValid", Boolean.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mac_algorithm (fun f ->
             "MacAlgorithm", MacAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.mac_valid (fun f -> "MacValid", Boolean.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; mac_valid = Aws.Util.option_map (Aws.Json.lookup j "MacValid") Boolean.of_json
  ; mac_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "MacAlgorithm") MacAlgorithmSpec.of_json
  }
