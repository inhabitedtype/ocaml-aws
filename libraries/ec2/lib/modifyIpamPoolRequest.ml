open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; description : String.t option
  ; auto_import : Boolean.t option
  ; allocation_min_netmask_length : Integer.t option
  ; allocation_max_netmask_length : Integer.t option
  ; allocation_default_netmask_length : Integer.t option
  ; clear_allocation_default_netmask_length : Boolean.t option
  ; add_allocation_resource_tags : RequestIpamResourceTagList.t
  ; remove_allocation_resource_tags : RequestIpamResourceTagList.t
  }

let make
    ?dry_run
    ~ipam_pool_id
    ?description
    ?auto_import
    ?allocation_min_netmask_length
    ?allocation_max_netmask_length
    ?allocation_default_netmask_length
    ?clear_allocation_default_netmask_length
    ?(add_allocation_resource_tags = [])
    ?(remove_allocation_resource_tags = [])
    () =
  { dry_run
  ; ipam_pool_id
  ; description
  ; auto_import
  ; allocation_min_netmask_length
  ; allocation_max_netmask_length
  ; allocation_default_netmask_length
  ; clear_allocation_default_netmask_length
  ; add_allocation_resource_tags
  ; remove_allocation_resource_tags
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; auto_import = Aws.Util.option_bind (Aws.Xml.member "AutoImport" xml) Boolean.parse
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
    ; clear_allocation_default_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "ClearAllocationDefaultNetmaskLength" xml)
          Boolean.parse
    ; add_allocation_resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddAllocationResourceTag" xml)
             RequestIpamResourceTagList.parse)
    ; remove_allocation_resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveAllocationResourceTag" xml)
             RequestIpamResourceTagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RemoveAllocationResourceTag"
              , RequestIpamResourceTagList.to_query v.remove_allocation_resource_tags ))
       ; Some
           (Aws.Query.Pair
              ( "AddAllocationResourceTag"
              , RequestIpamResourceTagList.to_query v.add_allocation_resource_tags ))
       ; Aws.Util.option_map v.clear_allocation_default_netmask_length (fun f ->
             Aws.Query.Pair ("ClearAllocationDefaultNetmaskLength", Boolean.to_query f))
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationDefaultNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMaxNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMinNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.auto_import (fun f ->
             Aws.Query.Pair ("AutoImport", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "RemoveAllocationResourceTag"
           , RequestIpamResourceTagList.to_json v.remove_allocation_resource_tags )
       ; Some
           ( "AddAllocationResourceTag"
           , RequestIpamResourceTagList.to_json v.add_allocation_resource_tags )
       ; Aws.Util.option_map v.clear_allocation_default_netmask_length (fun f ->
             "ClearAllocationDefaultNetmaskLength", Boolean.to_json f)
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             "AllocationDefaultNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             "AllocationMaxNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             "AllocationMinNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.auto_import (fun f -> "AutoImport", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; auto_import = Aws.Util.option_map (Aws.Json.lookup j "AutoImport") Boolean.of_json
  ; allocation_min_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "AllocationMinNetmaskLength") Integer.of_json
  ; allocation_max_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "AllocationMaxNetmaskLength") Integer.of_json
  ; allocation_default_netmask_length =
      Aws.Util.option_map
        (Aws.Json.lookup j "AllocationDefaultNetmaskLength")
        Integer.of_json
  ; clear_allocation_default_netmask_length =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClearAllocationDefaultNetmaskLength")
        Boolean.of_json
  ; add_allocation_resource_tags =
      RequestIpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddAllocationResourceTag"))
  ; remove_allocation_resource_tags =
      RequestIpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveAllocationResourceTag"))
  }
