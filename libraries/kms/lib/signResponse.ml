open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; signature : Blob.t option
  ; signing_algorithm : SigningAlgorithmSpec.t option
  }

let make ?key_id ?signature ?signing_algorithm () =
  { key_id; signature; signing_algorithm }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; signature = Aws.Util.option_bind (Aws.Xml.member "Signature" xml) Blob.parse
    ; signing_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "SigningAlgorithm" xml)
          SigningAlgorithmSpec.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.signing_algorithm (fun f ->
             Aws.Query.Pair ("SigningAlgorithm", SigningAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.signature (fun f ->
             Aws.Query.Pair ("Signature", Blob.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.signing_algorithm (fun f ->
             "SigningAlgorithm", SigningAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.signature (fun f -> "Signature", Blob.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; signature = Aws.Util.option_map (Aws.Json.lookup j "Signature") Blob.of_json
  ; signing_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "SigningAlgorithm")
        SigningAlgorithmSpec.of_json
  }
