open Aws.BaseTypes

type t =
  { ciphertext_blob : Blob.t option
  ; source_key_id : String.t option
  ; key_id : String.t option
  ; source_encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; destination_encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; source_key_material_id : String.t option
  ; destination_key_material_id : String.t option
  }

let make
    ?ciphertext_blob
    ?source_key_id
    ?key_id
    ?source_encryption_algorithm
    ?destination_encryption_algorithm
    ?source_key_material_id
    ?destination_key_material_id
    () =
  { ciphertext_blob
  ; source_key_id
  ; key_id
  ; source_encryption_algorithm
  ; destination_encryption_algorithm
  ; source_key_material_id
  ; destination_key_material_id
  }

let parse xml =
  Some
    { ciphertext_blob =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
    ; source_key_id = Aws.Util.option_bind (Aws.Xml.member "SourceKeyId" xml) String.parse
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; source_encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceEncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; destination_encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationEncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; source_key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "SourceKeyMaterialId" xml) String.parse
    ; destination_key_material_id =
        Aws.Util.option_bind (Aws.Xml.member "DestinationKeyMaterialId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_key_material_id (fun f ->
             Aws.Query.Pair ("DestinationKeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.source_key_material_id (fun f ->
             Aws.Query.Pair ("SourceKeyMaterialId", String.to_query f))
       ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
             Aws.Query.Pair
               ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
             Aws.Query.Pair
               ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Aws.Util.option_map v.source_key_id (fun f ->
             Aws.Query.Pair ("SourceKeyId", String.to_query f))
       ; Aws.Util.option_map v.ciphertext_blob (fun f ->
             Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_key_material_id (fun f ->
             "DestinationKeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.source_key_material_id (fun f ->
             "SourceKeyMaterialId", String.to_json f)
       ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
             "DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
             "SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Aws.Util.option_map v.source_key_id (fun f -> "SourceKeyId", String.to_json f)
       ; Aws.Util.option_map v.ciphertext_blob (fun f -> "CiphertextBlob", Blob.to_json f)
       ])

let of_json j =
  { ciphertext_blob =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
  ; source_key_id = Aws.Util.option_map (Aws.Json.lookup j "SourceKeyId") String.of_json
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; source_encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceEncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; destination_encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationEncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; source_key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceKeyMaterialId") String.of_json
  ; destination_key_material_id =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationKeyMaterialId") String.of_json
  }
