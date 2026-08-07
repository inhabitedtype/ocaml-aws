open Aws.BaseTypes

type t =
  { ipv6_cidr_block_association : SubnetIpv6CidrBlockAssociation.t option
  ; subnet_id : String.t option
  }

let make ?ipv6_cidr_block_association ?subnet_id () =
  { ipv6_cidr_block_association; subnet_id }

let parse xml =
  Some
    { ipv6_cidr_block_association =
        Aws.Util.option_bind
          (Aws.Xml.member "ipv6CidrBlockAssociation" xml)
          SubnetIpv6CidrBlockAssociation.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.ipv6_cidr_block_association (fun f ->
             Aws.Query.Pair
               ("Ipv6CidrBlockAssociation", SubnetIpv6CidrBlockAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.ipv6_cidr_block_association (fun f ->
             "ipv6CidrBlockAssociation", SubnetIpv6CidrBlockAssociation.to_json f)
       ])

let of_json j =
  { ipv6_cidr_block_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipv6CidrBlockAssociation")
        SubnetIpv6CidrBlockAssociation.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  }
