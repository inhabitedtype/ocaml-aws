open Aws.BaseTypes

type t =
  { tenant_id : String.t option
  ; public_signing_key_url : String.t option
  }

let make ?tenant_id ?public_signing_key_url () = { tenant_id; public_signing_key_url }

let parse xml =
  Some
    { tenant_id = Aws.Util.option_bind (Aws.Xml.member "TenantId" xml) String.parse
    ; public_signing_key_url =
        Aws.Util.option_bind (Aws.Xml.member "PublicSigningKeyUrl" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_signing_key_url (fun f ->
             Aws.Query.Pair ("PublicSigningKeyUrl", String.to_query f))
       ; Aws.Util.option_map v.tenant_id (fun f ->
             Aws.Query.Pair ("TenantId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_signing_key_url (fun f ->
             "PublicSigningKeyUrl", String.to_json f)
       ; Aws.Util.option_map v.tenant_id (fun f -> "TenantId", String.to_json f)
       ])

let of_json j =
  { tenant_id = Aws.Util.option_map (Aws.Json.lookup j "TenantId") String.of_json
  ; public_signing_key_url =
      Aws.Util.option_map (Aws.Json.lookup j "PublicSigningKeyUrl") String.of_json
  }
