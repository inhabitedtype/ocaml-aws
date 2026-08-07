open Aws.BaseTypes

type t =
  { ipv6_cidr_block_association : VpcIpv6CidrBlockAssociation.t option
  ; cidr_block_association : VpcCidrBlockAssociation.t option
  ; vpc_id : String.t option
  }

let make ?ipv6_cidr_block_association ?cidr_block_association ?vpc_id () =
  { ipv6_cidr_block_association; cidr_block_association; vpc_id }

let parse xml =
  Some
    { ipv6_cidr_block_association =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv6CidrBlockAssociation" xml)
          VpcIpv6CidrBlockAssociation.parse
    ; cidr_block_association =
        Aws.Util.option_bind
          (Aws.Xml.member "cidrBlockAssociation" xml)
          VpcCidrBlockAssociation.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.cidr_block_association (fun f ->
             Aws.Query.Pair ("CidrBlockAssociation", VpcCidrBlockAssociation.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block_association (fun f ->
             Aws.Query.Pair
               ("Ipv6CidrBlockAssociation", VpcIpv6CidrBlockAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.cidr_block_association (fun f ->
             "cidrBlockAssociation", VpcCidrBlockAssociation.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block_association (fun f ->
             "ipv6CidrBlockAssociation", VpcIpv6CidrBlockAssociation.to_json f)
       ])

let of_json j =
  { ipv6_cidr_block_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv6CidrBlockAssociation")
        VpcIpv6CidrBlockAssociation.of_json
  ; cidr_block_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "cidrBlockAssociation")
        VpcCidrBlockAssociation.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
