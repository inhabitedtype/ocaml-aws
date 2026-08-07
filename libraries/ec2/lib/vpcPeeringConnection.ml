open Aws.BaseTypes

type t =
  { accepter_vpc_info : VpcPeeringConnectionVpcInfo.t option
  ; expiration_time : DateTime.t option
  ; requester_vpc_info : VpcPeeringConnectionVpcInfo.t option
  ; status : VpcPeeringConnectionStateReason.t option
  ; tags : TagList.t
  ; vpc_peering_connection_id : String.t option
  }

let make
    ?accepter_vpc_info
    ?expiration_time
    ?requester_vpc_info
    ?status
    ?(tags = [])
    ?vpc_peering_connection_id
    () =
  { accepter_vpc_info
  ; expiration_time
  ; requester_vpc_info
  ; status
  ; tags
  ; vpc_peering_connection_id
  }

let parse xml =
  Some
    { accepter_vpc_info =
        Aws.Util.option_bind
          (Aws.Xml.member "accepterVpcInfo" xml)
          VpcPeeringConnectionVpcInfo.parse
    ; expiration_time =
        Aws.Util.option_bind (Aws.Xml.member "expirationTime" xml) DateTime.parse
    ; requester_vpc_info =
        Aws.Util.option_bind
          (Aws.Xml.member "requesterVpcInfo" xml)
          VpcPeeringConnectionVpcInfo.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          VpcPeeringConnectionStateReason.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_peering_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcPeeringConnectionId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             Aws.Query.Pair ("VpcPeeringConnectionId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", VpcPeeringConnectionStateReason.to_query f))
       ; Aws.Util.option_map v.requester_vpc_info (fun f ->
             Aws.Query.Pair ("RequesterVpcInfo", VpcPeeringConnectionVpcInfo.to_query f))
       ; Aws.Util.option_map v.expiration_time (fun f ->
             Aws.Query.Pair ("ExpirationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.accepter_vpc_info (fun f ->
             Aws.Query.Pair ("AccepterVpcInfo", VpcPeeringConnectionVpcInfo.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             "vpcPeeringConnectionId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f ->
             "status", VpcPeeringConnectionStateReason.to_json f)
       ; Aws.Util.option_map v.requester_vpc_info (fun f ->
             "requesterVpcInfo", VpcPeeringConnectionVpcInfo.to_json f)
       ; Aws.Util.option_map v.expiration_time (fun f ->
             "expirationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.accepter_vpc_info (fun f ->
             "accepterVpcInfo", VpcPeeringConnectionVpcInfo.to_json f)
       ])

let of_json j =
  { accepter_vpc_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "accepterVpcInfo")
        VpcPeeringConnectionVpcInfo.of_json
  ; expiration_time =
      Aws.Util.option_map (Aws.Json.lookup j "expirationTime") DateTime.of_json
  ; requester_vpc_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "requesterVpcInfo")
        VpcPeeringConnectionVpcInfo.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        VpcPeeringConnectionStateReason.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_peering_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcPeeringConnectionId") String.of_json
  }
