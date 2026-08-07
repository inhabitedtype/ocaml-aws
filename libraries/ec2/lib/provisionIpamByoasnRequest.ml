open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_id : String.t
  ; asn : String.t
  ; asn_authorization_context : AsnAuthorizationContext.t
  }

let make ?dry_run ~ipam_id ~asn ~asn_authorization_context () =
  { dry_run; ipam_id; asn; asn_authorization_context }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_id =
        Aws.Xml.required
          "IpamId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamId" xml) String.parse)
    ; asn =
        Aws.Xml.required
          "Asn"
          (Aws.Util.option_bind (Aws.Xml.member "Asn" xml) String.parse)
    ; asn_authorization_context =
        Aws.Xml.required
          "AsnAuthorizationContext"
          (Aws.Util.option_bind
             (Aws.Xml.member "AsnAuthorizationContext" xml)
             AsnAuthorizationContext.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AsnAuthorizationContext"
              , AsnAuthorizationContext.to_query v.asn_authorization_context ))
       ; Some (Aws.Query.Pair ("Asn", String.to_query v.asn))
       ; Some (Aws.Query.Pair ("IpamId", String.to_query v.ipam_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AsnAuthorizationContext"
           , AsnAuthorizationContext.to_json v.asn_authorization_context )
       ; Some ("Asn", String.to_json v.asn)
       ; Some ("IpamId", String.to_json v.ipam_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamId"))
  ; asn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Asn"))
  ; asn_authorization_context =
      AsnAuthorizationContext.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AsnAuthorizationContext"))
  }
