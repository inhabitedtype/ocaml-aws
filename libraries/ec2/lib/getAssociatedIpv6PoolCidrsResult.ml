open Aws.BaseTypes

type t =
  { ipv6_cidr_associations : Ipv6CidrAssociationSet.t
  ; next_token : String.t option
  }

let make ?(ipv6_cidr_associations = []) ?next_token () =
  { ipv6_cidr_associations; next_token }

let parse xml =
  Some
    { ipv6_cidr_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6CidrAssociationSet" xml)
             Ipv6CidrAssociationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "Ipv6CidrAssociationSet"
              , Ipv6CidrAssociationSet.to_query v.ipv6_cidr_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "ipv6CidrAssociationSet"
           , Ipv6CidrAssociationSet.to_json v.ipv6_cidr_associations )
       ])

let of_json j =
  { ipv6_cidr_associations =
      Ipv6CidrAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6CidrAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
