open Aws.BaseTypes

type t =
  { type_ : String.t option
  ; values : DataResourceValues.t
  }

let make ?type_ ?(values = []) () = { type_; values }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Values" xml) DataResourceValues.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Values.member", DataResourceValues.to_query v.values))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Values", DataResourceValues.to_json v.values)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", String.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") String.of_json
  ; values =
      DataResourceValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  }
