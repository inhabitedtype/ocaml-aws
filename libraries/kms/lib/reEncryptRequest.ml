open Aws.BaseTypes

type t =
  { ciphertext_blob : Blob.t option
  ; source_encryption_context : EncryptionContextType.t option
  ; source_key_id : String.t option
  ; destination_key_id : String.t
  ; destination_encryption_context : EncryptionContextType.t option
  ; source_encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; destination_encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; grant_tokens : GrantTokenList.t
  ; dry_run : Boolean.t option
  ; dry_run_modifiers : DryRunModifierList.t
  }

let make
    ?ciphertext_blob
    ?source_encryption_context
    ?source_key_id
    ~destination_key_id
    ?destination_encryption_context
    ?source_encryption_algorithm
    ?destination_encryption_algorithm
    ?(grant_tokens = [])
    ?dry_run
    ?(dry_run_modifiers = [])
    () =
  { ciphertext_blob
  ; source_encryption_context
  ; source_key_id
  ; destination_key_id
  ; destination_encryption_context
  ; source_encryption_algorithm
  ; destination_encryption_algorithm
  ; grant_tokens
  ; dry_run
  ; dry_run_modifiers
  }

let parse xml =
  Some
    { ciphertext_blob =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
    ; source_encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceEncryptionContext" xml)
          EncryptionContextType.parse
    ; source_key_id = Aws.Util.option_bind (Aws.Xml.member "SourceKeyId" xml) String.parse
    ; destination_key_id =
        Aws.Xml.required
          "DestinationKeyId"
          (Aws.Util.option_bind (Aws.Xml.member "DestinationKeyId" xml) String.parse)
    ; destination_encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationEncryptionContext" xml)
          EncryptionContextType.parse
    ; source_encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceEncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; destination_encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "DestinationEncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; dry_run_modifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DryRunModifiers" xml)
             DryRunModifierList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DryRunModifiers.member", DryRunModifierList.to_query v.dry_run_modifiers))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
             Aws.Query.Pair
               ("DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
             Aws.Query.Pair
               ("SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.destination_encryption_context (fun f ->
             Aws.Query.Pair
               ("DestinationEncryptionContext", EncryptionContextType.to_query f))
       ; Some (Aws.Query.Pair ("DestinationKeyId", String.to_query v.destination_key_id))
       ; Aws.Util.option_map v.source_key_id (fun f ->
             Aws.Query.Pair ("SourceKeyId", String.to_query f))
       ; Aws.Util.option_map v.source_encryption_context (fun f ->
             Aws.Query.Pair ("SourceEncryptionContext", EncryptionContextType.to_query f))
       ; Aws.Util.option_map v.ciphertext_blob (fun f ->
             Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DryRunModifiers", DryRunModifierList.to_json v.dry_run_modifiers)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Aws.Util.option_map v.destination_encryption_algorithm (fun f ->
             "DestinationEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.source_encryption_algorithm (fun f ->
             "SourceEncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.destination_encryption_context (fun f ->
             "DestinationEncryptionContext", EncryptionContextType.to_json f)
       ; Some ("DestinationKeyId", String.to_json v.destination_key_id)
       ; Aws.Util.option_map v.source_key_id (fun f -> "SourceKeyId", String.to_json f)
       ; Aws.Util.option_map v.source_encryption_context (fun f ->
             "SourceEncryptionContext", EncryptionContextType.to_json f)
       ; Aws.Util.option_map v.ciphertext_blob (fun f -> "CiphertextBlob", Blob.to_json f)
       ])

let of_json j =
  { ciphertext_blob =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
  ; source_encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceEncryptionContext")
        EncryptionContextType.of_json
  ; source_key_id = Aws.Util.option_map (Aws.Json.lookup j "SourceKeyId") String.of_json
  ; destination_key_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DestinationKeyId"))
  ; destination_encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationEncryptionContext")
        EncryptionContextType.of_json
  ; source_encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceEncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; destination_encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "DestinationEncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; dry_run_modifiers =
      DryRunModifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DryRunModifiers"))
  }
