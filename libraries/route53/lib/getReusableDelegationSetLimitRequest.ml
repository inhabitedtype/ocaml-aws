open Aws.BaseTypes

type t =
  { type_ : ReusableDelegationSetLimitType.t
  ; delegation_set_id : String.t
  }

let make ~type_ ~delegation_set_id () = { type_; delegation_set_id }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind
             (Aws.Xml.member "Type" xml)
             ReusableDelegationSetLimitType.parse)
    ; delegation_set_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Id", String.to_query v.delegation_set_id))
       ; Some (Aws.Query.Pair ("Type", ReusableDelegationSetLimitType.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Id", String.to_json v.delegation_set_id)
       ; Some ("Type", ReusableDelegationSetLimitType.to_json v.type_)
       ])

let of_json j =
  { type_ =
      ReusableDelegationSetLimitType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; delegation_set_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  }
