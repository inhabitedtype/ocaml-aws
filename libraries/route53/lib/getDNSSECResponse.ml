type t =
  { status : DNSSECStatus.t
  ; key_signing_keys : KeySigningKeys.t
  }

let make ~status ~key_signing_keys () = { status; key_signing_keys }

let parse xml =
  Some
    { status =
        Aws.Xml.required
          "Status"
          (Aws.Util.option_bind (Aws.Xml.member "Status" xml) DNSSECStatus.parse)
    ; key_signing_keys =
        Aws.Xml.required
          "KeySigningKeys"
          (Aws.Util.option_bind
             (Aws.Xml.member "KeySigningKeys" xml)
             KeySigningKeys.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("KeySigningKeys.member", KeySigningKeys.to_query v.key_signing_keys))
       ; Some (Aws.Query.Pair ("Status", DNSSECStatus.to_query v.status))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("KeySigningKeys", KeySigningKeys.to_json v.key_signing_keys)
       ; Some ("Status", DNSSECStatus.to_json v.status)
       ])

let of_json j =
  { status = DNSSECStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
  ; key_signing_keys =
      KeySigningKeys.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySigningKeys"))
  }
