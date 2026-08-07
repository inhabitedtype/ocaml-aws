open Aws.BaseTypes

type t =
  { key_id : String.t
  ; plaintext : Blob.t
  ; encryption_context : EncryptionContextType.t option
  ; grant_tokens : GrantTokenList.t
  ; encryption_algorithm : EncryptionAlgorithmSpec.t option
  ; dry_run : Boolean.t option
  }

let make
    ~key_id
    ~plaintext
    ?encryption_context
    ?(grant_tokens = [])
    ?encryption_algorithm
    ?dry_run
    () =
  { key_id; plaintext; encryption_context; grant_tokens; encryption_algorithm; dry_run }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; plaintext =
        Aws.Xml.required
          "Plaintext"
          (Aws.Util.option_bind (Aws.Xml.member "Plaintext" xml) Blob.parse)
    ; encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContext" xml)
          EncryptionContextType.parse
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; encryption_algorithm =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionAlgorithm" xml)
          EncryptionAlgorithmSpec.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             Aws.Query.Pair ("EncryptionAlgorithm", EncryptionAlgorithmSpec.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Aws.Util.option_map v.encryption_context (fun f ->
             Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
       ; Some (Aws.Query.Pair ("Plaintext", Blob.to_query v.plaintext))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.encryption_algorithm (fun f ->
             "EncryptionAlgorithm", EncryptionAlgorithmSpec.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Aws.Util.option_map v.encryption_context (fun f ->
             "EncryptionContext", EncryptionContextType.to_json f)
       ; Some ("Plaintext", Blob.to_json v.plaintext)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; plaintext = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Plaintext"))
  ; encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContext")
        EncryptionContextType.of_json
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; encryption_algorithm =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionAlgorithm")
        EncryptionAlgorithmSpec.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
