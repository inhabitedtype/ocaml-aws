open Aws.BaseTypes

type t =
  { mac : Blob.t option
  ; mac_algorithm : MacAlgorithmSpec.t option
  ; key_id : String.t option
  }

let make ?mac ?mac_algorithm ?key_id () = { mac; mac_algorithm; key_id }

let parse xml =
  Some
    { mac = Aws.Util.option_bind (Aws.Xml.member "Mac" xml) Blob.parse
    ; mac_algorithm =
        Aws.Util.option_bind (Aws.Xml.member "MacAlgorithm" xml) MacAlgorithmSpec.parse
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Aws.Util.option_map v.mac_algorithm (fun f ->
             Aws.Query.Pair ("MacAlgorithm", MacAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.mac (fun f -> Aws.Query.Pair ("Mac", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Aws.Util.option_map v.mac_algorithm (fun f ->
             "MacAlgorithm", MacAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.mac (fun f -> "Mac", Blob.to_json f)
       ])

let of_json j =
  { mac = Aws.Util.option_map (Aws.Json.lookup j "Mac") Blob.of_json
  ; mac_algorithm =
      Aws.Util.option_map (Aws.Json.lookup j "MacAlgorithm") MacAlgorithmSpec.of_json
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  }
