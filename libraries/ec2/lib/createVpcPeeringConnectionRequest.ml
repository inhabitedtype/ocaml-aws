open Aws.BaseTypes

type t =
  { peer_region : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; vpc_id : String.t
  ; peer_vpc_id : String.t option
  ; peer_owner_id : String.t option
  }

let make
    ?peer_region
    ?(tag_specifications = [])
    ?dry_run
    ~vpc_id
    ?peer_vpc_id
    ?peer_owner_id
    () =
  { peer_region; tag_specifications; dry_run; vpc_id; peer_vpc_id; peer_owner_id }

let parse xml =
  Some
    { peer_region = Aws.Util.option_bind (Aws.Xml.member "PeerRegion" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    ; peer_vpc_id = Aws.Util.option_bind (Aws.Xml.member "peerVpcId" xml) String.parse
    ; peer_owner_id = Aws.Util.option_bind (Aws.Xml.member "peerOwnerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_owner_id (fun f ->
             Aws.Query.Pair ("PeerOwnerId", String.to_query f))
       ; Aws.Util.option_map v.peer_vpc_id (fun f ->
             Aws.Query.Pair ("PeerVpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.peer_region (fun f ->
             Aws.Query.Pair ("PeerRegion", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peer_owner_id (fun f -> "peerOwnerId", String.to_json f)
       ; Aws.Util.option_map v.peer_vpc_id (fun f -> "peerVpcId", String.to_json f)
       ; Some ("vpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.peer_region (fun f -> "PeerRegion", String.to_json f)
       ])

let of_json j =
  { peer_region = Aws.Util.option_map (Aws.Json.lookup j "PeerRegion") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; peer_vpc_id = Aws.Util.option_map (Aws.Json.lookup j "peerVpcId") String.of_json
  ; peer_owner_id = Aws.Util.option_map (Aws.Json.lookup j "peerOwnerId") String.of_json
  }
