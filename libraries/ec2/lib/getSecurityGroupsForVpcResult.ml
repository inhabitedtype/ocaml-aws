open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; security_group_for_vpcs : SecurityGroupForVpcList.t
  }

let make ?next_token ?(security_group_for_vpcs = []) () =
  { next_token; security_group_for_vpcs }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; security_group_for_vpcs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupForVpcSet" xml)
             SecurityGroupForVpcList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecurityGroupForVpcSet"
              , SecurityGroupForVpcList.to_query v.security_group_for_vpcs ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "securityGroupForVpcSet"
           , SecurityGroupForVpcList.to_json v.security_group_for_vpcs )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; security_group_for_vpcs =
      SecurityGroupForVpcList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupForVpcSet"))
  }
