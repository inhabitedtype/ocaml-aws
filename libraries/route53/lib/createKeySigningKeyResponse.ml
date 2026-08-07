open Aws.BaseTypes

type t =
  { change_info : ChangeInfo.t
  ; key_signing_key : KeySigningKey.t
  ; location : String.t
  }

let make ~change_info ~key_signing_key ~location () =
  { change_info; key_signing_key; location }

let parse xml =
  Some
    { change_info =
        Aws.Xml.required
          "ChangeInfo"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeInfo" xml) ChangeInfo.parse)
    ; key_signing_key =
        Aws.Xml.required
          "KeySigningKey"
          (Aws.Util.option_bind (Aws.Xml.member "KeySigningKey" xml) KeySigningKey.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some (Aws.Query.Pair ("KeySigningKey", KeySigningKey.to_query v.key_signing_key))
       ; Some (Aws.Query.Pair ("ChangeInfo", ChangeInfo.to_query v.change_info))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("KeySigningKey", KeySigningKey.to_json v.key_signing_key)
       ; Some ("ChangeInfo", ChangeInfo.to_json v.change_info)
       ])

let of_json j =
  { change_info =
      ChangeInfo.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeInfo"))
  ; key_signing_key =
      KeySigningKey.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeySigningKey"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
