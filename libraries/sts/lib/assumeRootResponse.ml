open Aws.BaseTypes

type t =
  { credentials : Credentials.t option
  ; source_identity : String.t option
  }

let make ?credentials ?source_identity () = { credentials; source_identity }

let parse xml =
  Some
    { credentials =
        Aws.Util.option_bind (Aws.Xml.member "Credentials" xml) Credentials.parse
    ; source_identity =
        Aws.Util.option_bind (Aws.Xml.member "SourceIdentity" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             Aws.Query.Pair ("SourceIdentity", String.to_query f))
       ; Aws.Util.option_map v.credentials (fun f ->
             Aws.Query.Pair ("Credentials", Credentials.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_identity (fun f ->
             "SourceIdentity", String.to_json f)
       ; Aws.Util.option_map v.credentials (fun f -> "Credentials", Credentials.to_json f)
       ])

let of_json j =
  { credentials =
      Aws.Util.option_map (Aws.Json.lookup j "Credentials") Credentials.of_json
  ; source_identity =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIdentity") String.of_json
  }
