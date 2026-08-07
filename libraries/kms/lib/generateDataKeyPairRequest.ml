open Aws.BaseTypes

type t =
  { encryption_context : EncryptionContextType.t option
  ; key_id : String.t
  ; key_pair_spec : DataKeyPairSpec.t
  ; grant_tokens : GrantTokenList.t
  ; recipient : RecipientInfo.t option
  ; dry_run : Boolean.t option
  }

let make
    ?encryption_context
    ~key_id
    ~key_pair_spec
    ?(grant_tokens = [])
    ?recipient
    ?dry_run
    () =
  { encryption_context; key_id; key_pair_spec; grant_tokens; recipient; dry_run }

let parse xml =
  Some
    { encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContext" xml)
          EncryptionContextType.parse
    ; key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; key_pair_spec =
        Aws.Xml.required
          "KeyPairSpec"
          (Aws.Util.option_bind (Aws.Xml.member "KeyPairSpec" xml) DataKeyPairSpec.parse)
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; recipient =
        Aws.Util.option_bind (Aws.Xml.member "Recipient" xml) RecipientInfo.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.recipient (fun f ->
             Aws.Query.Pair ("Recipient", RecipientInfo.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Some (Aws.Query.Pair ("KeyPairSpec", DataKeyPairSpec.to_query v.key_pair_spec))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ; Aws.Util.option_map v.encryption_context (fun f ->
             Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.recipient (fun f -> "Recipient", RecipientInfo.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Some ("KeyPairSpec", DataKeyPairSpec.to_json v.key_pair_spec)
       ; Some ("KeyId", String.to_json v.key_id)
       ; Aws.Util.option_map v.encryption_context (fun f ->
             "EncryptionContext", EncryptionContextType.to_json f)
       ])

let of_json j =
  { encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContext")
        EncryptionContextType.of_json
  ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; key_pair_spec =
      DataKeyPairSpec.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyPairSpec"))
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; recipient = Aws.Util.option_map (Aws.Json.lookup j "Recipient") RecipientInfo.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
