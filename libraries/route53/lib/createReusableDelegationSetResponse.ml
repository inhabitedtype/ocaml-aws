open Aws.BaseTypes

type t =
  { delegation_set : DelegationSet.t
  ; location : String.t
  }

let make ~delegation_set ~location () = { delegation_set; location }

let parse xml =
  Some
    { delegation_set =
        Aws.Xml.required
          "DelegationSet"
          (Aws.Util.option_bind (Aws.Xml.member "DelegationSet" xml) DelegationSet.parse)
    ; location =
        Aws.Xml.required
          "Location"
          (Aws.Util.option_bind (Aws.Xml.member "Location" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Location", String.to_query v.location))
       ; Some (Aws.Query.Pair ("DelegationSet", DelegationSet.to_query v.delegation_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Location", String.to_json v.location)
       ; Some ("DelegationSet", DelegationSet.to_json v.delegation_set)
       ])

let of_json j =
  { delegation_set =
      DelegationSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DelegationSet"))
  ; location = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Location"))
  }
