type t =
  { type_ : Type.t
  ; equals : OperatorTargetList.t
  }

let make ~type_ ~equals () = { type_; equals }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) Type.parse)
    ; equals =
        Aws.Xml.required
          "Equals"
          (Aws.Util.option_bind (Aws.Xml.member "Equals" xml) OperatorTargetList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Equals.member", OperatorTargetList.to_query v.equals))
       ; Some (Aws.Query.Pair ("Type", Type.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Equals", OperatorTargetList.to_json v.equals)
       ; Some ("Type", Type.to_json v.type_)
       ])

let of_json j =
  { type_ = Type.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; equals =
      OperatorTargetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Equals"))
  }
