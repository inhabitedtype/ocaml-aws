open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; cidr_block : String.t option
  ; state : SecondaryNetworkCidrBlockAssociationState.t option
  ; state_reason : String.t option
  }

let make ?association_id ?cidr_block ?state ?state_reason () =
  { association_id; cidr_block; state; state_reason }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          SecondaryNetworkCidrBlockAssociationState.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SecondaryNetworkCidrBlockAssociationState.to_query f))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_reason (fun f -> "stateReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", SecondaryNetworkCidrBlockAssociationState.to_json f)
       ; Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        SecondaryNetworkCidrBlockAssociationState.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "stateReason") String.of_json
  }
