open Aws.BaseTypes

type t =
  { ciphertext_blob : Blob.t option
  ; encryption_context : EncryptionContextType.t option
  ; grant_tokens : GrantTokenList.t
  ; key_id : String.t option
  ; encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; recipient : RecipientInfo.t option
  ; dry_run : Boolean.t option
  ; dry_run_modifiers : DryRunModifierList.t
  }

let make
    ?ciphertext_blob
    ?encryption_context
    ?(grant_tokens = [])
    ?key_id
    ?encryption_algorithm
    ?recipient
    ?dry_run
    ?(dry_run_modifiers = [])
    () =
  { ciphertext_blob
  ; encryption_context
  ; grant_tokens
  ; key_id
  ; encryption_algorithm
  ; recipient
  ; dry_run
  ; dry_run_modifiers
  }

let parse xml =
  Some
    { ciphertext_blob =
        Aws.Util.option_bind (Aws.Xml.member "CiphertextBlob" xml) Blob.parse
    ; encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContext" xml)
          EncryptionContextType.parse
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; recipient =
        Aws.Util.option_bind (Aws.Xml.member "Recipient" xml) RecipientInfo.parse
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
       ; Aws.Util.option_map v.recipient (fun f ->
             Aws.Query.Pair ("Recipient", RecipientInfo.to_query f))
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Aws.Util.option_map v.encryption_context (fun f ->
             Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
       ; Aws.Util.option_map v.ciphertext_blob (fun f ->
             Aws.Query.Pair ("CiphertextBlob", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DryRunModifiers", DryRunModifierList.to_json v.dry_run_modifiers)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.recipient (fun f -> "Recipient", RecipientInfo.to_json f)
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Aws.Util.option_map v.encryption_context (fun f ->
             "EncryptionContext", EncryptionContextType.to_json f)
       ; Aws.Util.option_map v.ciphertext_blob (fun f -> "CiphertextBlob", Blob.to_json f)
       ])

let of_json j =
  { ciphertext_blob =
      Aws.Util.option_map (Aws.Json.lookup j "CiphertextBlob") Blob.of_json
  ; encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContext")
        EncryptionContextType.of_json
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; recipient = Aws.Util.option_map (Aws.Json.lookup j "Recipient") RecipientInfo.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; dry_run_modifiers =
      DryRunModifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DryRunModifiers"))
  }
