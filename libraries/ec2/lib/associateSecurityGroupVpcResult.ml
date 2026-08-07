type t = { state : SecurityGroupVpcAssociationState.t option }

let make ?state () = { state }

let parse xml =
  Some
    { state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          SecurityGroupVpcAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SecurityGroupVpcAssociationState.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", SecurityGroupVpcAssociationState.to_json f)
       ])

let of_json j =
  { state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        SecurityGroupVpcAssociationState.of_json
  }
