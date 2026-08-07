open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_scope_id : String.t
  ; locale : String.t option
  ; source_ipam_pool_id : String.t option
  ; description : String.t option
  ; address_family : AddressFamily.t
  ; auto_import : Boolean.t option
  ; publicly_advertisable : Boolean.t option
  ; allocation_min_netmask_length : Integer.t option
  ; allocation_max_netmask_length : Integer.t option
  ; allocation_default_netmask_length : Integer.t option
  ; allocation_resource_tags : RequestIpamResourceTagList.t
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; aws_service : IpamPoolAwsService.t option
  ; public_ip_source : IpamPoolPublicIpSource.t option
  ; source_resource : IpamPoolSourceResourceRequest.t option
  }

let make
    ?dry_run
    ~ipam_scope_id
    ?locale
    ?source_ipam_pool_id
    ?description
    ~address_family
    ?auto_import
    ?publicly_advertisable
    ?allocation_min_netmask_length
    ?allocation_max_netmask_length
    ?allocation_default_netmask_length
    ?(allocation_resource_tags = [])
    ?(tag_specifications = [])
    ?client_token
    ?aws_service
    ?public_ip_source
    ?source_resource
    () =
  { dry_run
  ; ipam_scope_id
  ; locale
  ; source_ipam_pool_id
  ; description
  ; address_family
  ; auto_import
  ; publicly_advertisable
  ; allocation_min_netmask_length
  ; allocation_max_netmask_length
  ; allocation_default_netmask_length
  ; allocation_resource_tags
  ; tag_specifications
  ; client_token
  ; aws_service
  ; public_ip_source
  ; source_resource
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_scope_id =
        Aws.Xml.required
          "IpamScopeId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamScopeId" xml) String.parse)
    ; locale = Aws.Util.option_bind (Aws.Xml.member "Locale" xml) String.parse
    ; source_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "SourceIpamPoolId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; address_family =
        Aws.Xml.required
          "AddressFamily"
          (Aws.Util.option_bind (Aws.Xml.member "AddressFamily" xml) AddressFamily.parse)
    ; auto_import = Aws.Util.option_bind (Aws.Xml.member "AutoImport" xml) Boolean.parse
    ; publicly_advertisable =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAdvertisable" xml) Boolean.parse
    ; allocation_min_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "AllocationMinNetmaskLength" xml)
          Integer.parse
    ; allocation_max_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "AllocationMaxNetmaskLength" xml)
          Integer.parse
    ; allocation_default_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "AllocationDefaultNetmaskLength" xml)
          Integer.parse
    ; allocation_resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AllocationResourceTag" xml)
             RequestIpamResourceTagList.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; aws_service =
        Aws.Util.option_bind (Aws.Xml.member "AwsService" xml) IpamPoolAwsService.parse
    ; public_ip_source =
        Aws.Util.option_bind
          (Aws.Xml.member "PublicIpSource" xml)
          IpamPoolPublicIpSource.parse
    ; source_resource =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceResource" xml)
          IpamPoolSourceResourceRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_resource (fun f ->
             Aws.Query.Pair ("SourceResource", IpamPoolSourceResourceRequest.to_query f))
       ; Aws.Util.option_map v.public_ip_source (fun f ->
             Aws.Query.Pair ("PublicIpSource", IpamPoolPublicIpSource.to_query f))
       ; Aws.Util.option_map v.aws_service (fun f ->
             Aws.Query.Pair ("AwsService", IpamPoolAwsService.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "AllocationResourceTag"
              , RequestIpamResourceTagList.to_query v.allocation_resource_tags ))
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationDefaultNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMaxNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMinNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             Aws.Query.Pair ("PubliclyAdvertisable", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_import (fun f ->
             Aws.Query.Pair ("AutoImport", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("AddressFamily", AddressFamily.to_query v.address_family))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             Aws.Query.Pair ("SourceIpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamScopeId", String.to_query v.ipam_scope_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_resource (fun f ->
             "SourceResource", IpamPoolSourceResourceRequest.to_json f)
       ; Aws.Util.option_map v.public_ip_source (fun f ->
             "PublicIpSource", IpamPoolPublicIpSource.to_json f)
       ; Aws.Util.option_map v.aws_service (fun f ->
             "AwsService", IpamPoolAwsService.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "AllocationResourceTag"
           , RequestIpamResourceTagList.to_json v.allocation_resource_tags )
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             "AllocationDefaultNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             "AllocationMaxNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             "AllocationMinNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             "PubliclyAdvertisable", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_import (fun f -> "AutoImport", Boolean.to_json f)
       ; Some ("AddressFamily", AddressFamily.to_json v.address_family)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             "SourceIpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.locale (fun f -> "Locale", String.to_json f)
       ; Some ("IpamScopeId", String.to_json v.ipam_scope_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_scope_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamScopeId"))
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "Locale") String.of_json
  ; source_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIpamPoolId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; address_family =
      AddressFamily.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AddressFamily"))
  ; auto_import = Aws.Util.option_map (Aws.Json.lookup j "AutoImport") Boolean.of_json
  ; publicly_advertisable =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAdvertisable") Boolean.of_json
  ; allocation_min_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "AllocationMinNetmaskLength") Integer.of_json
  ; allocation_max_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "AllocationMaxNetmaskLength") Integer.of_json
  ; allocation_default_netmask_length =
      Aws.Util.option_map
        (Aws.Json.lookup j "AllocationDefaultNetmaskLength")
        Integer.of_json
  ; allocation_resource_tags =
      RequestIpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationResourceTag"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; aws_service =
      Aws.Util.option_map (Aws.Json.lookup j "AwsService") IpamPoolAwsService.of_json
  ; public_ip_source =
      Aws.Util.option_map
        (Aws.Json.lookup j "PublicIpSource")
        IpamPoolPublicIpSource.of_json
  ; source_resource =
      Aws.Util.option_map
        (Aws.Json.lookup j "SourceResource")
        IpamPoolSourceResourceRequest.of_json
  }
