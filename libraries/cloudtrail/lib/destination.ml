open Aws.BaseTypes

type t =
  { type_ : DestinationType.t
  ; location : String.t
  }

let make ~type_ ~location () = { type_; location }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) DestinationType.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some (Aws.Query.Pair ("Type", DestinationType.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("Type", DestinationType.to_json v.type_)
       ])

let of_json j =
  { type_ = DestinationType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
