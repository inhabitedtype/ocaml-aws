open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; plaintext : Blob.t option
  ; encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; ciphertext_for_recipient : Blob.t option
  ; key_material_id : String.t option
  }

let make
    ?key_id
    ?plaintext
    ?encryption_algorithm
    ?ciphertext_for_recipient
    ?key_material_id
    () =
  { key_id; plaintext; encryption_algorithm; ciphertext_for_recipient; key_material_id }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; plaintext = Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse
    ; encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; ciphertext_for_recipient =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextForRecipient" xml) Blob.parse
    ; key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "KeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             Aws.Query.Pair ("KeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             Aws.Query.Pair ("CiphertextForRecipient", Blob.to_query f))
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.plaintext (fun f ->
             Aws.Query.Pair ("Plaintext", Blob.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             "CiphertextForRecipient", Blob.to_json f)
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.plaintext (fun f -> "Plaintext", Blob.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; plaintext = Aws.Util.option_map (Aws.Json.lookup j "Plaintext") Blob.of_json
  ; encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; ciphertext_for_recipient =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextForRecipient") Blob.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  }
