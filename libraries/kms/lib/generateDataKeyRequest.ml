open Aws.BaseTypes

type t =
  { key_id : String.t
  ; encryption_context : EncryptionContextType.t option
  ; number_of_bytes : Integer.t option
  ; key_spec : DataKeySpec.t option
  ; grant_tokens : GrantTokenList.t
  ; recipient : RecipientInfo.t option
  ; dry_run : Boolean.t option
  }

let make
    ~key_id
    ?encryption_context
    ?number_of_bytes
    ?key_spec
    ?(grant_tokens = [])
    ?recipient
    ?dry_run
    () =
  { key_id
  ; encryption_context
  ; number_of_bytes
  ; key_spec
  ; grant_tokens
  ; recipient
  ; dry_run
  }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; encryption_context =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContext" xml)
          EncryptionContextType.parse
    ; number_of_bytes =
        Aws.Util.option_bind (Aws.Xml.member "NumberOfBytes" xml) Integer.parse
    ; key_spec = Aws.Util.option_bind (Aws.Xml.member "KeySpec" xml) DataKeySpec.parse
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
       ; Aws.Util.option_map v.key_spec (fun f ->
             Aws.Query.Pair ("KeySpec", DataKeySpec.to_query f))
       ; Aws.Util.option_map v.number_of_bytes (fun f ->
             Aws.Query.Pair ("NumberOfBytes", Integer.to_query f))
       ; Aws.Util.option_map v.encryption_context (fun f ->
             Aws.Query.Pair ("EncryptionContext", EncryptionContextType.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.recipient (fun f -> "Recipient", RecipientInfo.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Aws.Util.option_map v.key_spec (fun f -> "KeySpec", DataKeySpec.to_json f)
       ; Aws.Util.option_map v.number_of_bytes (fun f ->
             "NumberOfBytes", Integer.to_json f)
       ; Aws.Util.option_map v.encryption_context (fun f ->
             "EncryptionContext", EncryptionContextType.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; encryption_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContext")
        EncryptionContextType.of_json
  ; number_of_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "NumberOfBytes") Integer.of_json
  ; key_spec = Aws.Util.option_map (Aws.Json.lookup j "KeySpec") DataKeySpec.of_json
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; recipient = Aws.Util.option_map (Aws.Json.lookup j "Recipient") RecipientInfo.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
