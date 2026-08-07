open Aws.BaseTypes

type t = { public_signing_key_url : String.t option }

let make ?public_signing_key_url () = { public_signing_key_url }

let parse xml =
  Some
    { public_signing_key_url =
        Aws.Util.option_bind (Aws.Xml.member "PublicSigningKeyUrl" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_signing_key_url (fun f ->
             Aws.Query.Pair ("PublicSigningKeyUrl", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_signing_key_url (fun f ->
             "PublicSigningKeyUrl", String.to_json f)
       ])

let of_json j =
  { public_signing_key_url =
      Aws.Util.option_map (Aws.Json.lookup j "PublicSigningKeyUrl") String.of_json
  }
