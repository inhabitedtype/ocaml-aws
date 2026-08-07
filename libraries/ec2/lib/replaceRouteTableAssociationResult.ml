open Aws.BaseTypes

type t =
  { new_association_id : String.t option
  ; association_state : RouteTableAssociationState.t option
  }

let make ?new_association_id ?association_state () =
  { new_association_id; association_state }

let parse xml =
  Some
    { new_association_id =
        Aws.Util.option_bind (Aws.Xml.member "newAssociationId" xml) String.parse
    ; association_state =
        Aws.Util.option_bind
          (Aws.Xml.member "associationState" xml)
          RouteTableAssociationState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association_state (fun f ->
             Aws.Query.Pair ("AssociationState", RouteTableAssociationState.to_query f))
       ; Aws.Util.option_map v.new_association_id (fun f ->
             Aws.Query.Pair ("NewAssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.association_state (fun f ->
             "associationState", RouteTableAssociationState.to_json f)
       ; Aws.Util.option_map v.new_association_id (fun f ->
             "newAssociationId", String.to_json f)
       ])

let of_json j =
  { new_association_id =
      Aws.Util.option_map (Aws.Json.lookup j "newAssociationId") String.of_json
  ; association_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "associationState")
        RouteTableAssociationState.of_json
  }
