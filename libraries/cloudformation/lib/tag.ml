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
          "Key"
          (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
    ; value =
        Aws.Xml.required
          "Value"
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
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
       [ Some ("Value", String.to_json v.value); Some ("Key", String.to_json v.key) ])

let of_json j =
  { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
  ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  }
