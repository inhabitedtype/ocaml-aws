open Aws.BaseTypes

type t =
  { d_b_subnet_group_name : String.t option
  ; d_b_subnet_group_description : String.t option
  ; vpc_id : String.t option
  ; subnet_group_status : String.t option
  ; subnets : SubnetList.t
  ; d_b_subnet_group_arn : String.t option
  ; supported_network_types : StringList.t
  }

let make
    ?d_b_subnet_group_name
    ?d_b_subnet_group_description
    ?vpc_id
    ?subnet_group_status
    ?(subnets = [])
    ?d_b_subnet_group_arn
    ?(supported_network_types = [])
    () =
  { d_b_subnet_group_name
  ; d_b_subnet_group_description
  ; vpc_id
  ; subnet_group_status
  ; subnets
  ; d_b_subnet_group_arn
  ; supported_network_types
  }

let parse xml =
  Some
    { d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; d_b_subnet_group_description =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupDescription" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; subnet_group_status =
        Aws.Util.option_bind (Aws.Xml.member "SubnetGroupStatus" xml) String.parse
    ; subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) SubnetList.parse)
    ; d_b_subnet_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupArn" xml) String.parse
    ; supported_network_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedNetworkTypes" xml)
             StringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedNetworkTypes.member"
              , StringList.to_query v.supported_network_types ))
       ; Aws.Util.option_map v.d_b_subnet_group_arn (fun f ->
             Aws.Query.Pair ("DBSubnetGroupArn", String.to_query f))
       ; Some (Aws.Query.Pair ("Subnets.member", SubnetList.to_query v.subnets))
       ; Aws.Util.option_map v.subnet_group_status (fun f ->
             Aws.Query.Pair ("SubnetGroupStatus", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group_description (fun f ->
             Aws.Query.Pair ("DBSubnetGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SupportedNetworkTypes", StringList.to_json v.supported_network_types)
       ; Aws.Util.option_map v.d_b_subnet_group_arn (fun f ->
             "DBSubnetGroupArn", String.to_json f)
       ; Some ("Subnets", SubnetList.to_json v.subnets)
       ; Aws.Util.option_map v.subnet_group_status (fun f ->
             "SubnetGroupStatus", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group_description (fun f ->
             "DBSubnetGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ])

let of_json j =
  { d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; d_b_subnet_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupDescription") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; subnet_group_status =
      Aws.Util.option_map (Aws.Json.lookup j "SubnetGroupStatus") String.of_json
  ; subnets = SubnetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
  ; d_b_subnet_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupArn") String.of_json
  ; supported_network_types =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedNetworkTypes"))
  }
