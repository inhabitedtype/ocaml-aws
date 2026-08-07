open Aws.BaseTypes

type t =
  { key_id : String.t
  ; message : Blob.t
  ; message_type : MessageType.t option
  ; signature : Blob.t
  ; signing_algorithm : SigningAlgorithmSpec.t
  ; grant_tokens : GrantTokenList.t
  ; dry_run : Boolean.t option
  }

let make
    ~key_id
    ~message
    ?message_type
    ~signature
    ~signing_algorithm
    ?(grant_tokens = [])
    ?dry_run
    () =
  { key_id; message; message_type; signature; signing_algorithm; grant_tokens; dry_run }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; message =
        Aws.Xml.required
          "Message"
          (Aws.Util.option_bind (Aws.Xml.member "Message" xml) Blob.parse)
    ; message_type =
        Aws.Util.option_bind (Aws.Xml.member "MessageType" xml) MessageType.parse
    ; signature =
        Aws.Xml.required
          "Signature"
          (Aws.Util.option_bind (Aws.Xml.member "Signature" xml) Blob.parse)
    ; signing_algorithm =
        Aws.Xml.required
          "SigningAlgorithm"
          (Aws.Util.option_bind
             (Aws.Xml.member "SigningAlgorithm" xml)
             SigningAlgorithmSpec.parse)
    ; grant_tokens =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "GrantTokens" xml) GrantTokenList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("GrantTokens.member", GrantTokenList.to_query v.grant_tokens))
       ; Some
           (Aws.Query.Pair
              ("SigningAlgorithm", SigningAlgorithmSpec.to_query v.signing_algorithm))
       ; Some (Aws.Query.Pair ("Signature", Blob.to_query v.signature))
       ; Aws.Util.option_map v.message_type (fun f ->
             Aws.Query.Pair ("MessageType", MessageType.to_query f))
       ; Some (Aws.Query.Pair ("Message", Blob.to_query v.message))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Some ("SigningAlgorithm", SigningAlgorithmSpec.to_json v.signing_algorithm)
       ; Some ("Signature", Blob.to_json v.signature)
       ; Aws.Util.option_map v.message_type (fun f ->
             "MessageType", MessageType.to_json f)
       ; Some ("Message", Blob.to_json v.message)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; message = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
  ; message_type =
      Aws.Util.option_map (Aws.Json.lookup j "MessageType") MessageType.of_json
  ; signature = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Signature"))
  ; signing_algorithm =
      SigningAlgorithmSpec.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SigningAlgorithm"))
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
