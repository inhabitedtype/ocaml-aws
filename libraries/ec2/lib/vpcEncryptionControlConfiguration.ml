type t =
  { mode : VpcEncryptionControlMode.t
  ; internet_gateway_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; egress_only_internet_gateway_exclusion :
      VpcEncryptionControlExclusionStateInput.t option
  ; nat_gateway_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; virtual_private_gateway_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; vpc_peering_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; lambda_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; vpc_lattice_exclusion : VpcEncryptionControlExclusionStateInput.t option
  ; elastic_file_system_exclusion : VpcEncryptionControlExclusionStateInput.t option
  }

let make
    ~mode
    ?internet_gateway_exclusion
    ?egress_only_internet_gateway_exclusion
    ?nat_gateway_exclusion
    ?virtual_private_gateway_exclusion
    ?vpc_peering_exclusion
    ?lambda_exclusion
    ?vpc_lattice_exclusion
    ?elastic_file_system_exclusion
    () =
  { mode
  ; internet_gateway_exclusion
  ; egress_only_internet_gateway_exclusion
  ; nat_gateway_exclusion
  ; virtual_private_gateway_exclusion
  ; vpc_peering_exclusion
  ; lambda_exclusion
  ; vpc_lattice_exclusion
  ; elastic_file_system_exclusion
  }

let parse xml =
  Some
    { mode =
        Aws.Xml.required
          "Mode"
          (Aws.Util.option_bind
             (Aws.Xml.member "Mode" xml)
             VpcEncryptionControlMode.parse)
    ; internet_gateway_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "InternetGatewayExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; egress_only_internet_gateway_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "EgressOnlyInternetGatewayExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; nat_gateway_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "NatGatewayExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; virtual_private_gateway_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "VirtualPrivateGatewayExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; vpc_peering_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "VpcPeeringExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; lambda_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "LambdaExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; vpc_lattice_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "VpcLatticeExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    ; elastic_file_system_exclusion =
        Aws.Util.option_bind
          (Aws.Xml.member "ElasticFileSystemExclusion" xml)
          VpcEncryptionControlExclusionStateInput.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_file_system_exclusion (fun f ->
             Aws.Query.Pair
               ( "ElasticFileSystemExclusion"
               , VpcEncryptionControlExclusionStateInput.to_query f ))
       ; Aws.Util.option_map v.vpc_lattice_exclusion (fun f ->
             Aws.Query.Pair
               ("VpcLatticeExclusion", VpcEncryptionControlExclusionStateInput.to_query f))
       ; Aws.Util.option_map v.lambda_exclusion (fun f ->
             Aws.Query.Pair
               ("LambdaExclusion", VpcEncryptionControlExclusionStateInput.to_query f))
       ; Aws.Util.option_map v.vpc_peering_exclusion (fun f ->
             Aws.Query.Pair
               ("VpcPeeringExclusion", VpcEncryptionControlExclusionStateInput.to_query f))
       ; Aws.Util.option_map v.virtual_private_gateway_exclusion (fun f ->
             Aws.Query.Pair
               ( "VirtualPrivateGatewayExclusion"
               , VpcEncryptionControlExclusionStateInput.to_query f ))
       ; Aws.Util.option_map v.nat_gateway_exclusion (fun f ->
             Aws.Query.Pair
               ("NatGatewayExclusion", VpcEncryptionControlExclusionStateInput.to_query f))
       ; Aws.Util.option_map v.egress_only_internet_gateway_exclusion (fun f ->
             Aws.Query.Pair
               ( "EgressOnlyInternetGatewayExclusion"
               , VpcEncryptionControlExclusionStateInput.to_query f ))
       ; Aws.Util.option_map v.internet_gateway_exclusion (fun f ->
             Aws.Query.Pair
               ( "InternetGatewayExclusion"
               , VpcEncryptionControlExclusionStateInput.to_query f ))
       ; Some (Aws.Query.Pair ("Mode", VpcEncryptionControlMode.to_query v.mode))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_file_system_exclusion (fun f ->
             ( "ElasticFileSystemExclusion"
             , VpcEncryptionControlExclusionStateInput.to_json f ))
       ; Aws.Util.option_map v.vpc_lattice_exclusion (fun f ->
             "VpcLatticeExclusion", VpcEncryptionControlExclusionStateInput.to_json f)
       ; Aws.Util.option_map v.lambda_exclusion (fun f ->
             "LambdaExclusion", VpcEncryptionControlExclusionStateInput.to_json f)
       ; Aws.Util.option_map v.vpc_peering_exclusion (fun f ->
             "VpcPeeringExclusion", VpcEncryptionControlExclusionStateInput.to_json f)
       ; Aws.Util.option_map v.virtual_private_gateway_exclusion (fun f ->
             ( "VirtualPrivateGatewayExclusion"
             , VpcEncryptionControlExclusionStateInput.to_json f ))
       ; Aws.Util.option_map v.nat_gateway_exclusion (fun f ->
             "NatGatewayExclusion", VpcEncryptionControlExclusionStateInput.to_json f)
       ; Aws.Util.option_map v.egress_only_internet_gateway_exclusion (fun f ->
             ( "EgressOnlyInternetGatewayExclusion"
             , VpcEncryptionControlExclusionStateInput.to_json f ))
       ; Aws.Util.option_map v.internet_gateway_exclusion (fun f ->
             "InternetGatewayExclusion", VpcEncryptionControlExclusionStateInput.to_json f)
       ; Some ("Mode", VpcEncryptionControlMode.to_json v.mode)
       ])

let of_json j =
  { mode =
      VpcEncryptionControlMode.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Mode"))
  ; internet_gateway_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "InternetGatewayExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; egress_only_internet_gateway_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "EgressOnlyInternetGatewayExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; nat_gateway_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "NatGatewayExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; virtual_private_gateway_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "VirtualPrivateGatewayExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; vpc_peering_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "VpcPeeringExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; lambda_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "LambdaExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; vpc_lattice_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "VpcLatticeExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  ; elastic_file_system_exclusion =
      Aws.Util.option_map
        (Aws.Json.lookup j "ElasticFileSystemExclusion")
        VpcEncryptionControlExclusionStateInput.of_json
  }
