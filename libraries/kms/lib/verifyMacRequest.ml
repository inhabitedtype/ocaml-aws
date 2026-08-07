open Aws.BaseTypes

type t =
  { message : Blob.t
  ; key_id : String.t
  ; mac_algorithm : MacAlgorithmSpec.t
  ; mac : Blob.t
  ; grant_tokens : GrantTokenList.t
  ; dry_run : Boolean.t option
  }

let make ~message ~key_id ~mac_algorithm ~mac ?(grant_tokens = []) ?dry_run () =
  { message; key_id; mac_algorithm; mac; grant_tokens; dry_run }

let parse xml =
  Some
    { message =
        Aws.Xml.required
          "Message"
          (Aws.Util.option_bind (Aws.Xml.member "Message" xml) Blob.parse)
    ; key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; mac_algorithm =
        Aws.Xml.required
          "MacAlgorithm"
          (Aws.Util.option_bind
             (Aws.Xml.member "MacAlgorithm" xml)
             MacAlgorithmSpec.parse)
    ; mac =
        Aws.Xml.required
          "Mac"
          (Aws.Util.option_bind (Aws.Xml.member "Mac" xml) Blob.parse)
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
       ; Some (Aws.Query.Pair ("Mac", Blob.to_query v.mac))
       ; Some (Aws.Query.Pair ("MacAlgorithm", MacAlgorithmSpec.to_query v.mac_algorithm))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ; Some (Aws.Query.Pair ("Message", Blob.to_query v.message))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("GrantTokens", GrantTokenList.to_json v.grant_tokens)
       ; Some ("Mac", Blob.to_json v.mac)
       ; Some ("MacAlgorithm", MacAlgorithmSpec.to_json v.mac_algorithm)
       ; Some ("KeyId", String.to_json v.key_id)
       ; Some ("Message", Blob.to_json v.message)
       ])

let of_json j =
  { message = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Message"))
  ; key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; mac_algorithm =
      MacAlgorithmSpec.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MacAlgorithm"))
  ; mac = Blob.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Mac"))
  ; grant_tokens =
      GrantTokenList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GrantTokens"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
