open Aws.BaseTypes

type t =
  { private_key_ciphertext_blob : Blob.t option
  ; private_key_plaintext : Blob.t option
  ; public_key : Blob.t option
  ; key_id : String.t option
  ; key_pair_spec : DataKeyPairSpec.t option
  ; ciphertext_for_recipient : Blob.t option
  ; key_material_id : String.t option
  }

let make
    ?private_key_ciphertext_blob
    ?private_key_plaintext
    ?public_key
    ?key_id
    ?key_pair_spec
    ?ciphertext_for_recipient
    ?key_material_id
    () =
  { private_key_ciphertext_blob
  ; private_key_plaintext
  ; public_key
  ; key_id
  ; key_pair_spec
  ; ciphertext_for_recipient
  ; key_material_id
  }

let parse xml =
  Some
    { private_key_ciphertext_blob =
        Aws.Util.option_bind (Aws.Xml.member "PrivateKeyCiphertextBlob" xml) Blob.parse
    ; private_key_plaintext =
        Aws.Util.option_bind (Aws.Xml.member "PrivateKeyPlaintext" xml) Blob.parse
    ; public_key = Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; key_pair_spec =
        Aws.Util.option_bind (Aws.Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse
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
       ; Aws.Util.option_map v.key_pair_spec (fun f ->
             Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Aws.Util.option_map v.public_key (fun f ->
             Aws.Query.Pair ("PublicKey", Blob.to_query f))
       ; Aws.Util.option_map v.private_key_plaintext (fun f ->
             Aws.Query.Pair ("PrivateKeyPlaintext", Blob.to_query f))
       ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
             Aws.Query.Pair ("PrivateKeyCiphertextBlob", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_material_id (fun f ->
             "KeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.ciphertext_for_recipient (fun f ->
             "CiphertextForRecipient", Blob.to_json f)
       ; Aws.Util.option_map v.key_pair_spec (fun f ->
             "KeyPairSpec", DataKeyPairSpec.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Aws.Util.option_map v.public_key (fun f -> "PublicKey", Blob.to_json f)
       ; Aws.Util.option_map v.private_key_plaintext (fun f ->
             "PrivateKeyPlaintext", Blob.to_json f)
       ; Aws.Util.option_map v.private_key_ciphertext_blob (fun f ->
             "PrivateKeyCiphertextBlob", Blob.to_json f)
       ])

let of_json j =
  { private_key_ciphertext_blob =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateKeyCiphertextBlob") Blob.of_json
  ; private_key_plaintext =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateKeyPlaintext") Blob.of_json
  ; public_key = Aws.Util.option_map (Aws.Json.lookup j "PublicKey") Blob.of_json
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; key_pair_spec =
      Aws.Util.option_map (Aws.Json.lookup j "KeyPairSpec") DataKeyPairSpec.of_json
  ; ciphertext_for_recipient =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextForRecipient") Blob.of_json
  ; key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "KeyMaterialId") String.of_json
  }
