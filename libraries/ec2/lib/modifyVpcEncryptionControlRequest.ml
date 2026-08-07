open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_encryption_control_id : String.t
  ; mode : VpcEncryptionControlMode.t option
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
    ?dry_run
    ~vpc_encryption_control_id
    ?mode
    ?internet_gateway_exclusion
    ?egress_only_internet_gateway_exclusion
    ?nat_gateway_exclusion
    ?virtual_private_gateway_exclusion
    ?vpc_peering_exclusion
    ?lambda_exclusion
    ?vpc_lattice_exclusion
    ?elastic_file_system_exclusion
    () =
  { dry_run
  ; vpc_encryption_control_id
  ; mode
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
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; vpc_encryption_control_id =
        Aws.Xml.required
          "VpcEncryptionControlId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcEncryptionControlId" xml)
             String.parse)
    ; mode =
        Aws.Util.option_bind (Aws.Xml.member "Mode" xml) VpcEncryptionControlMode.parse
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
       ; Aws.Util.option_map v.mode (fun f ->
             Aws.Query.Pair ("Mode", VpcEncryptionControlMode.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VpcEncryptionControlId", String.to_query v.vpc_encryption_control_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
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
       ; Aws.Util.option_map v.mode (fun f -> "Mode", VpcEncryptionControlMode.to_json f)
       ; Some ("VpcEncryptionControlId", String.to_json v.vpc_encryption_control_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; vpc_encryption_control_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcEncryptionControlId"))
  ; mode = Aws.Util.option_map (Aws.Json.lookup j "Mode") VpcEncryptionControlMode.of_json
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
