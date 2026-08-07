open Aws.BaseTypes

type t =
  { identifier : String.t
  ; type_ : String.t option
  }

let make ~identifier ?type_ () = { identifier; type_ }

let parse xml =
  Some
    { identifier =
        Aws.Xml.required
          "Identifier"
          (Aws.Util.option_bind (Aws.Xml.member "Identifier" xml) String.parse)
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ; Some (Aws.Query.Pair ("Identifier", String.to_query v.identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "Type", String.to_json f)
       ; Some ("Identifier", String.to_json v.identifier)
       ])

let of_json j =
  { identifier = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Identifier"))
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") String.of_json
  }
