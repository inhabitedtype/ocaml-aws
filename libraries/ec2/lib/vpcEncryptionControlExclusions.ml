type t =
  { internet_gateway : VpcEncryptionControlExclusion.t option
  ; egress_only_internet_gateway : VpcEncryptionControlExclusion.t option
  ; nat_gateway : VpcEncryptionControlExclusion.t option
  ; virtual_private_gateway : VpcEncryptionControlExclusion.t option
  ; vpc_peering : VpcEncryptionControlExclusion.t option
  ; lambda : VpcEncryptionControlExclusion.t option
  ; vpc_lattice : VpcEncryptionControlExclusion.t option
  ; elastic_file_system : VpcEncryptionControlExclusion.t option
  }

let make
    ?internet_gateway
    ?egress_only_internet_gateway
    ?nat_gateway
    ?virtual_private_gateway
    ?vpc_peering
    ?lambda
    ?vpc_lattice
    ?elastic_file_system
    () =
  { internet_gateway
  ; egress_only_internet_gateway
  ; nat_gateway
  ; virtual_private_gateway
  ; vpc_peering
  ; lambda
  ; vpc_lattice
  ; elastic_file_system
  }

let parse xml =
  Some
    { internet_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "internetGateway" xml)
          VpcEncryptionControlExclusion.parse
    ; egress_only_internet_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "egressOnlyInternetGateway" xml)
          VpcEncryptionControlExclusion.parse
    ; nat_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "natGateway" xml)
          VpcEncryptionControlExclusion.parse
    ; virtual_private_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "virtualPrivateGateway" xml)
          VpcEncryptionControlExclusion.parse
    ; vpc_peering =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcPeering" xml)
          VpcEncryptionControlExclusion.parse
    ; lambda =
        Aws.Util.option_bind
          (Aws.Xml.member "lambda" xml)
          VpcEncryptionControlExclusion.parse
    ; vpc_lattice =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcLattice" xml)
          VpcEncryptionControlExclusion.parse
    ; elastic_file_system =
        Aws.Util.option_bind
          (Aws.Xml.member "elasticFileSystem" xml)
          VpcEncryptionControlExclusion.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_file_system (fun f ->
             Aws.Query.Pair ("ElasticFileSystem", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.vpc_lattice (fun f ->
             Aws.Query.Pair ("VpcLattice", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.lambda (fun f ->
             Aws.Query.Pair ("Lambda", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.vpc_peering (fun f ->
             Aws.Query.Pair ("VpcPeering", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.virtual_private_gateway (fun f ->
             Aws.Query.Pair
               ("VirtualPrivateGateway", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.nat_gateway (fun f ->
             Aws.Query.Pair ("NatGateway", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.egress_only_internet_gateway (fun f ->
             Aws.Query.Pair
               ("EgressOnlyInternetGateway", VpcEncryptionControlExclusion.to_query f))
       ; Aws.Util.option_map v.internet_gateway (fun f ->
             Aws.Query.Pair ("InternetGateway", VpcEncryptionControlExclusion.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.elastic_file_system (fun f ->
             "elasticFileSystem", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.vpc_lattice (fun f ->
             "vpcLattice", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.lambda (fun f ->
             "lambda", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.vpc_peering (fun f ->
             "vpcPeering", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.virtual_private_gateway (fun f ->
             "virtualPrivateGateway", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.nat_gateway (fun f ->
             "natGateway", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.egress_only_internet_gateway (fun f ->
             "egressOnlyInternetGateway", VpcEncryptionControlExclusion.to_json f)
       ; Aws.Util.option_map v.internet_gateway (fun f ->
             "internetGateway", VpcEncryptionControlExclusion.to_json f)
       ])

let of_json j =
  { internet_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "internetGateway")
        VpcEncryptionControlExclusion.of_json
  ; egress_only_internet_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "egressOnlyInternetGateway")
        VpcEncryptionControlExclusion.of_json
  ; nat_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "natGateway")
        VpcEncryptionControlExclusion.of_json
  ; virtual_private_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "virtualPrivateGateway")
        VpcEncryptionControlExclusion.of_json
  ; vpc_peering =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcPeering")
        VpcEncryptionControlExclusion.of_json
  ; lambda =
      Aws.Util.option_map
        (Aws.Json.lookup j "lambda")
        VpcEncryptionControlExclusion.of_json
  ; vpc_lattice =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcLattice")
        VpcEncryptionControlExclusion.of_json
  ; elastic_file_system =
      Aws.Util.option_map
        (Aws.Json.lookup j "elasticFileSystem")
        VpcEncryptionControlExclusion.of_json
  }
