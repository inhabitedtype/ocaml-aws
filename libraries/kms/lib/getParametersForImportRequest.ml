open Aws.BaseTypes

type t =
  { key_id : String.t
  ; wrapping_algorithm : AlgorithmSpec.t
  ; wrapping_key_spec : WrappingKeySpec.t
  }

let make ~key_id ~wrapping_algorithm ~wrapping_key_spec () =
  { key_id; wrapping_algorithm; wrapping_key_spec }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; wrapping_algorithm =
        Aws.Xml.required
          "WrappingAlgorithm"
          (Aws.Util.option_bind
             (Aws.Xml.member "WrappingAlgorithm" xml)
             AlgorithmSpec.parse)
    ; wrapping_key_spec =
        Aws.Xml.required
          "WrappingKeySpec"
          (Aws.Util.option_bind
             (Aws.Xml.member "WrappingKeySpec" xml)
             WrappingKeySpec.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("WrappingKeySpec", WrappingKeySpec.to_query v.wrapping_key_spec))
       ; Some
           (Aws.Query.Pair
              ("WrappingAlgorithm", AlgorithmSpec.to_query v.wrapping_algorithm))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("WrappingKeySpec", WrappingKeySpec.to_json v.wrapping_key_spec)
       ; Some ("WrappingAlgorithm", AlgorithmSpec.to_json v.wrapping_algorithm)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; wrapping_algorithm =
      AlgorithmSpec.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "WrappingAlgorithm"))
  ; wrapping_key_spec =
      WrappingKeySpec.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "WrappingKeySpec"))
  }
