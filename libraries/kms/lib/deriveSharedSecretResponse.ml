open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; shared_secret : Blob.t option
  ; ciphertext_for_recipient : Blob.t option
  ; key_agreement_algorithm : KeyAgreementAlgorithmSpec.t option
  ; key_origin : OriginType.t option
  }

let make
    ?key_id
    ?shared_secret
    ?ciphertext_for_recipient
    ?key_agreement_algorithm
    ?key_origin
    () =
  { key_id; shared_secret; ciphertext_for_recipient; key_agreement_algorithm; key_origin }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; shared_secret = Aws.Util.option_bind (Aws.Xml.member "SharedSecret" xml) Blob.parse
    ; ciphertext_for_recipient =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextForRecipient" xml) Blob.parse
    ; key_agreement_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "KeyAgreementAlgorithm" xml)
          KeyAgreementAlgorithmSpec.parse
    ; key_origin = Aws.Util.option_bind (Aws.Xml.member "KeyOrigin" xml) OriginType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_origin (fun f ->
             Aws.Query.Pair ("KeyOrigin", OriginType.to_query f))
       ; Aws.Util.option_map v.key_agreement_algorithm (fun f ->
             Aws.Query.Pair ("KeyAgreementAlgorithm", KeyAgreementAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             Aws.Query.Pair ("CiphertextForRecipient", Blob.to_query f))
       ; Aws.Util.option_map v.shared_secret (fun f ->
             Aws.Query.Pair ("SharedSecret", Blob.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_origin (fun f -> "KeyOrigin", OriginType.to_json f)
       ; Aws.Util.option_map v.key_agreement_algorithm (fun f ->
             "KeyAgreementAlgorithm", KeyAgreementAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             "CiphertextForRecipient", Blob.to_json f)
       ; Aws.Util.option_map v.shared_secret (fun f -> "SharedSecret", Blob.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; shared_secret = Aws.Util.option_map (Aws.Json.lookup j "SharedSecret") Blob.of_json
  ; ciphertext_for_recipient =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextForRecipient") Blob.of_json
  ; key_agreement_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "KeyAgreementAlgorithm")
        KeyAgreementAlgorithmSpec.of_json
  ; key_origin = Aws.Util.option_map (Aws.Json.lookup j "KeyOrigin") OriginType.of_json
  }
