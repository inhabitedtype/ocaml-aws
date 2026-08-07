open Aws.BaseTypes

type t =
  { key_id : String.t
  ; primary_region : String.t
  }

let make ~key_id ~primary_region () = { key_id; primary_region }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; primary_region =
        Aws.Xml.required
          "PrimaryRegion"
          (Aws.Util.option_bind (Aws.Xml.member "PrimaryRegion" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PrimaryRegion", String.to_query v.primary_region))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("PrimaryRegion", String.to_json v.primary_region)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; primary_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryRegion"))
  }
