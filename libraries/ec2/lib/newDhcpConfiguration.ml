open Aws.BaseTypes

type t =
  { key : String.t option
  ; values : ValueStringList.t
  }

let make ?key ?(values = []) () = { key; values }

let parse xml =
  Some
    { key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Value" xml) ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Value", ValueStringList.to_query v.values))
       ; Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Value", ValueStringList.to_json v.values)
       ; Aws.Util.option_map v.key (fun f -> "Key", String.to_json f)
       ])

let of_json j =
  { key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json
  ; values = ValueStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
  }
