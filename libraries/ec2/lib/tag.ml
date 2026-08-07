open Aws.BaseTypes

type t =
  { key : String.t
  ; value : String.t
  }

let make ~key ~value () = { key; value }

let parse xml =
  Some
    { key =
        Aws.Xml.required
          "key"
          (Aws.Util.option_bind (Aws.Xml.member "key" xml) String.parse)
    ; value =
        Aws.Xml.required
          "value"
          (Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
       ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("value", String.to_json v.value); Some ("key", String.to_json v.key) ])

let of_json j =
  { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "key"))
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "value"))
  }
