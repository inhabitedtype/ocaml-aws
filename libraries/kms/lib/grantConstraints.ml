open Aws.BaseTypes

type t =
  { encryption_context_subset : EncryptionContextType.t option
  ; encryption_context_equals : EncryptionContextType.t option
  ; source_arn : String.t option
  }

let make ?encryption_context_subset ?encryption_context_equals ?source_arn () =
  { encryption_context_subset; encryption_context_equals; source_arn }

let parse xml =
  Some
    { encryption_context_subset =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContextSubset" xml)
          EncryptionContextType.parse
    ; encryption_context_equals =
        Aws.Util.option_bind
          (Aws.Xml.member "EncryptionContextEquals" xml)
          EncryptionContextType.parse
    ; source_arn = Aws.Util.option_bind (Aws.Xml.member "SourceArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_arn (fun f ->
             Aws.Query.Pair ("SourceArn", String.to_query f))
       ; Aws.Util.option_map v.encryption_context_equals (fun f ->
             Aws.Query.Pair ("EncryptionContextEquals", EncryptionContextType.to_query f))
       ; Aws.Util.option_map v.encryption_context_subset (fun f ->
             Aws.Query.Pair ("EncryptionContextSubset", EncryptionContextType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_arn (fun f -> "SourceArn", String.to_json f)
       ; Aws.Util.option_map v.encryption_context_equals (fun f ->
             "EncryptionContextEquals", EncryptionContextType.to_json f)
       ; Aws.Util.option_map v.encryption_context_subset (fun f ->
             "EncryptionContextSubset", EncryptionContextType.to_json f)
       ])

let of_json j =
  { encryption_context_subset =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContextSubset")
        EncryptionContextType.of_json
  ; encryption_context_equals =
      Aws.Util.option_map
        (Aws.Json.lookup j "EncryptionContextEquals")
        EncryptionContextType.of_json
  ; source_arn = Aws.Util.option_map (Aws.Json.lookup j "SourceArn") String.of_json
  }
