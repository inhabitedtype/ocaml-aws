open Aws.BaseTypes

type t =
  { security_group_vpc_associations : SecurityGroupVpcAssociationList.t
  ; next_token : String.t option
  }

let make ?(security_group_vpc_associations = []) ?next_token () =
  { security_group_vpc_associations; next_token }

let parse xml =
  Some
    { security_group_vpc_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupVpcAssociationSet" xml)
             SecurityGroupVpcAssociationList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupVpcAssociationSet"
              , SecurityGroupVpcAssociationList.to_query v.security_group_vpc_associations
              ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "securityGroupVpcAssociationSet"
           , SecurityGroupVpcAssociationList.to_json v.security_group_vpc_associations )
       ])

let of_json j =
  { security_group_vpc_associations =
      SecurityGroupVpcAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupVpcAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
