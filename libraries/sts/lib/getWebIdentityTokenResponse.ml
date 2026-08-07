open Aws.BaseTypes

type t =
  { web_identity_token : String.t option
  ; expiration : DateTime.t option
  }

let make ?web_identity_token ?expiration () = { web_identity_token; expiration }

let parse xml =
  Some
    { web_identity_token =
        Aws.Util.option_bind (Aws.Xml.member "WebIdentityToken" xml) String.parse
    ; expiration = Aws.Util.option_bind (Aws.Xml.member "Expiration" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expiration (fun f ->
             Aws.Query.Pair ("Expiration", DateTime.to_query f))
       ; Aws.Util.option_map v.web_identity_token (fun f ->
             Aws.Query.Pair ("WebIdentityToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.expiration (fun f -> "Expiration", DateTime.to_json f)
       ; Aws.Util.option_map v.web_identity_token (fun f ->
             "WebIdentityToken", String.to_json f)
       ])

let of_json j =
  { web_identity_token =
      Aws.Util.option_map (Aws.Json.lookup j "WebIdentityToken") String.of_json
  ; expiration = Aws.Util.option_map (Aws.Json.lookup j "Expiration") DateTime.of_json
  }
