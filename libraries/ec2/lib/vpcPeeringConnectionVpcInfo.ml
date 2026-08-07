open Aws.BaseTypes

type t =
  { cidr_block : String.t option
  ; ipv6_cidr_block_set : Ipv6CidrBlockSet.t
  ; cidr_block_set : CidrBlockSet.t
  ; owner_id : String.t option
  ; peering_options : VpcPeeringConnectionOptionsDescription.t option
  ; vpc_id : String.t option
  ; region : String.t option
  }

let make
    ?cidr_block
    ?(ipv6_cidr_block_set = [])
    ?(cidr_block_set = [])
    ?owner_id
    ?peering_options
    ?vpc_id
    ?region
    () =
  { cidr_block
  ; ipv6_cidr_block_set
  ; cidr_block_set
  ; owner_id
  ; peering_options
  ; vpc_id
  ; region
  }

let parse xml =
  Some
    { cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse
    ; ipv6_cidr_block_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6CidrBlockSet" xml)
             Ipv6CidrBlockSet.parse)
    ; cidr_block_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "cidrBlockSet" xml) CidrBlockSet.parse)
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; peering_options =
        Aws.Util.option_bind
          (Aws.Xml.member "peeringOptions" xml)
          VpcPeeringConnectionOptionsDescription.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "region" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.peering_options (fun f ->
             Aws.Query.Pair
               ("PeeringOptions", VpcPeeringConnectionOptionsDescription.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Some (Aws.Query.Pair ("CidrBlockSet", CidrBlockSet.to_query v.cidr_block_set))
       ; Some
           (Aws.Query.Pair
              ("Ipv6CidrBlockSet", Ipv6CidrBlockSet.to_query v.ipv6_cidr_block_set))
       ; Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region (fun f -> "region", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.peering_options (fun f ->
             "peeringOptions", VpcPeeringConnectionOptionsDescription.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Some ("cidrBlockSet", CidrBlockSet.to_json v.cidr_block_set)
       ; Some ("ipv6CidrBlockSet", Ipv6CidrBlockSet.to_json v.ipv6_cidr_block_set)
       ; Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f)
       ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json
  ; ipv6_cidr_block_set =
      Ipv6CidrBlockSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6CidrBlockSet"))
  ; cidr_block_set =
      CidrBlockSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrBlockSet"))
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; peering_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "peeringOptions")
        VpcPeeringConnectionOptionsDescription.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "region") String.of_json
  }
