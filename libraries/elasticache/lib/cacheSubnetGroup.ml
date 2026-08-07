open Aws.BaseTypes

type t =
  { cache_subnet_group_name : String.t option
  ; cache_subnet_group_description : String.t option
  ; vpc_id : String.t option
  ; subnets : SubnetList.t
  ; a_r_n : String.t option
  ; supported_network_types : NetworkTypeList.t
  }

let make
    ?cache_subnet_group_name
    ?cache_subnet_group_description
    ?vpc_id
    ?(subnets = [])
    ?a_r_n
    ?(supported_network_types = [])
    () =
  { cache_subnet_group_name
  ; cache_subnet_group_description
  ; vpc_id
  ; subnets
  ; a_r_n
  ; supported_network_types
  }

let parse xml =
  Some
    { cache_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
    ; cache_subnet_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheSubnetGroupDescription" xml)
          String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) SubnetList.parse)
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    ; supported_network_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedNetworkTypes" xml)
             NetworkTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedNetworkTypes.member"
              , NetworkTypeList.to_query v.supported_network_types ))
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Some (Aws.Query.Pair ("Subnets.member", SubnetList.to_query v.subnets))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SupportedNetworkTypes", NetworkTypeList.to_json v.supported_network_types)
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Some ("Subnets", SubnetList.to_json v.subnets)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
             "CacheSubnetGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             "CacheSubnetGroupName", String.to_json f)
       ])

let of_json j =
  { cache_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
  ; cache_subnet_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupDescription") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; subnets = SubnetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  ; supported_network_types =
      NetworkTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedNetworkTypes"))
  }
