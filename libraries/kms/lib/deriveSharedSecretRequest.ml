open Aws.BaseTypes

type t =
  { key_id : String.t
  ; key_agreement_algorithm : KeyAgreementAlgorithmSpec.t
  ; public_key : Blob.t
  ; grant_tokens : GrantTokenList.t
  ; dry_run : Boolean.t option
  ; recipient : RecipientInfo.t option
  }

let make
    ~key_id
    ~key_agreement_algorithm
    ~public_key
    ?(grant_tokens = [])
    ?dry_run
    ?recipient
    () =
  { key_id; key_agreement_algorithm; public_key; grant_tokens; dry_run; recipient }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; key_agreement_algorithm =
        Aws.Xml.required
          "KeyAgreementAlgorithm"
          (Aws.Util.option_bind
             (Aws.Xml.member "KeyAgreementAlgorithm" xml)
             KeyAgreementAlgorithmSpec.parse)
    ; public_key =
        Aws.Xml.required
          "PublicKey"
          (Aws.Util.option_bind (Aws.Xml.member "PublicKey" xml) Blob.parse)
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; recipient =
        Aws.Util.option_bind (Aws.Xml.member "Recipient" xml) RecipientInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recipient (fun f ->
             Aws.Query.Pair ("Recipient", RecipientInfo.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Some (Aws.Query.Pair ("PublicKey", Blob.to_query v.public_key))
       ; Some
           (Aws.Query.Pair
              ( "KeyAgreementAlgorithm"
              , KeyAgreementAlgorithmSpec.to_query v.key_agreement_algorithm ))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recipient (fun f -> "Recipient", RecipientInfo.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Some ("PublicKey", Blob.to_json v.public_key)
       ; Some
           ( "KeyAgreementAlgorithm"
           , KeyAgreementAlgorithmSpec.to_json v.key_agreement_algorithm )
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; key_agreement_algorithm =
      KeyAgreementAlgorithmSpec.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyAgreementAlgorithm"))
  ; public_key = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PublicKey"))
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; recipient = Aws.Util.option_map (Aws.Json.lookup j "Recipient") RecipientInfo.of_json
  }
