open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; instance_tenancy : Tenancy.t
  ; ipv6_cidr_block_association_set : VpcIpv6CidrBlockAssociationSet.t
  ; cidr_block_association_set : VpcCidrBlockAssociationSet.t
  ; is_default : Boolean.t
  ; encryption_control : VpcEncryptionControl.t option
  ; tags : TagList.t
  ; block_public_access_states : BlockPublicAccessStates.t option
  ; vpc_id : String.t
  ; state : VpcState.t
  ; cidr_block : String.t
  ; dhcp_options_id : String.t
  }

let make
    ?owner_id
    ~instance_tenancy
    ?(ipv6_cidr_block_association_set = [])
    ?(cidr_block_association_set = [])
    ~is_default
    ?encryption_control
    ?(tags = [])
    ?block_public_access_states
    ~vpc_id
    ~state
    ~cidr_block
    ~dhcp_options_id
    () =
  { owner_id
  ; instance_tenancy
  ; ipv6_cidr_block_association_set
  ; cidr_block_association_set
  ; is_default
  ; encryption_control
  ; tags
  ; block_public_access_states
  ; vpc_id
  ; state
  ; cidr_block
  ; dhcp_options_id
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; instance_tenancy =
        Aws.Xml.required
          "instanceTenancy"
          (Aws.Util.option_bind (Aws.Xml.member "instanceTenancy" xml) Tenancy.parse)
    ; ipv6_cidr_block_association_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6CidrBlockAssociationSet" xml)
             VpcIpv6CidrBlockAssociationSet.parse)
    ; cidr_block_association_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "cidrBlockAssociationSet" xml)
             VpcCidrBlockAssociationSet.parse)
    ; is_default =
        Aws.Xml.required
          "isDefault"
          (Aws.Util.option_bind (Aws.Xml.member "isDefault" xml) Boolean.parse)
    ; encryption_control =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionControl" xml)
          VpcEncryptionControl.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; block_public_access_states =
        Aws.Util.option_bind
          (Aws.Xml.member "blockPublicAccessStates" xml)
          BlockPublicAccessStates.parse
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) VpcState.parse)
    ; cidr_block =
        Aws.Xml.required
          "cidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse)
    ; dhcp_options_id =
        Aws.Xml.required
          "dhcpOptionsId"
          (Aws.Util.option_bind (Aws.Xml.member "dhcpOptionsId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DhcpOptionsId", String.to_query v.dhcp_options_id))
       ; Some (Aws.Query.Pair ("CidrBlock", String.to_query v.cidr_block))
       ; Some (Aws.Query.Pair ("State", VpcState.to_query v.state))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.block_public_access_states (fun f ->
             Aws.Query.Pair ("BlockPublicAccessStates", BlockPublicAccessStates.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.encryption_control (fun f ->
             Aws.Query.Pair ("EncryptionControl", VpcEncryptionControl.to_query f))
       ; Some (Aws.Query.Pair ("IsDefault", Boolean.to_query v.is_default))
       ; Some
           (Aws.Query.Pair
              ( "CidrBlockAssociationSet"
              , VpcCidrBlockAssociationSet.to_query v.cidr_block_association_set ))
       ; Some
           (Aws.Query.Pair
              ( "Ipv6CidrBlockAssociationSet"
              , VpcIpv6CidrBlockAssociationSet.to_query v.ipv6_cidr_block_association_set
              ))
       ; Some (Aws.Query.Pair ("InstanceTenancy", Tenancy.to_query v.instance_tenancy))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("dhcpOptionsId", String.to_json v.dhcp_options_id)
       ; Some ("cidrBlock", String.to_json v.cidr_block)
       ; Some ("state", VpcState.to_json v.state)
       ; Some ("vpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.block_public_access_states (fun f ->
             "blockPublicAccessStates", BlockPublicAccessStates.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.encryption_control (fun f ->
             "encryptionControl", VpcEncryptionControl.to_json f)
       ; Some ("isDefault", Boolean.to_json v.is_default)
       ; Some
           ( "cidrBlockAssociationSet"
           , VpcCidrBlockAssociationSet.to_json v.cidr_block_association_set )
       ; Some
           ( "ipv6CidrBlockAssociationSet"
           , VpcIpv6CidrBlockAssociationSet.to_json v.ipv6_cidr_block_association_set )
       ; Some ("instanceTenancy", Tenancy.to_json v.instance_tenancy)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; instance_tenancy =
      Tenancy.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceTenancy"))
  ; ipv6_cidr_block_association_set =
      VpcIpv6CidrBlockAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6CidrBlockAssociationSet"))
  ; cidr_block_association_set =
      VpcCidrBlockAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrBlockAssociationSet"))
  ; is_default = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "isDefault"))
  ; encryption_control =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionControl")
        VpcEncryptionControl.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; block_public_access_states =
      Aws.Util.option_map
        (Aws.Json.lookup j "blockPublicAccessStates")
        BlockPublicAccessStates.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; state = VpcState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; cidr_block = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "cidrBlock"))
  ; dhcp_options_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "dhcpOptionsId"))
  }
