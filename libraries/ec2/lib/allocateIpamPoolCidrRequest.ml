open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; cidr : String.t option
  ; netmask_length : Integer.t option
  ; client_token : String.t option
  ; description : String.t option
  ; preview_next_cidr : Boolean.t option
  ; allowed_cidrs : IpamPoolAllocationAllowedCidrs.t
  ; disallowed_cidrs : IpamPoolAllocationDisallowedCidrs.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ~ipam_pool_id
    ?cidr
    ?netmask_length
    ?client_token
    ?description
    ?preview_next_cidr
    ?(allowed_cidrs = [])
    ?(disallowed_cidrs = [])
    ?(tag_specifications = [])
    () =
  { dry_run
  ; ipam_pool_id
  ; cidr
  ; netmask_length
  ; client_token
  ; description
  ; preview_next_cidr
  ; allowed_cidrs
  ; disallowed_cidrs
  ; tag_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse
    ; netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "NetmaskLength" xml) Integer.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; preview_next_cidr =
        Aws.Util.option_bind (Aws.Xml.member "PreviewNextCidr" xml) Boolean.parse
    ; allowed_cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AllowedCidr" xml)
             IpamPoolAllocationAllowedCidrs.parse)
    ; disallowed_cidrs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DisallowedCidr" xml)
             IpamPoolAllocationDisallowedCidrs.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "DisallowedCidr"
              , IpamPoolAllocationDisallowedCidrs.to_query v.disallowed_cidrs ))
       ; Some
           (Aws.Query.Pair
              ("AllowedCidr", IpamPoolAllocationAllowedCidrs.to_query v.allowed_cidrs))
       ; Aws.Util.option_map v.preview_next_cidr (fun f ->
             Aws.Query.Pair ("PreviewNextCidr", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.netmask_length (fun f ->
             Aws.Query.Pair ("NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ("DisallowedCidr", IpamPoolAllocationDisallowedCidrs.to_json v.disallowed_cidrs)
       ; Some ("AllowedCidr", IpamPoolAllocationAllowedCidrs.to_json v.allowed_cidrs)
       ; Aws.Util.option_map v.preview_next_cidr (fun f ->
             "PreviewNextCidr", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.netmask_length (fun f ->
             "NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "Cidr", String.to_json f)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "Cidr") String.of_json
  ; netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "NetmaskLength") Integer.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; preview_next_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "PreviewNextCidr") Boolean.of_json
  ; allowed_cidrs =
      IpamPoolAllocationAllowedCidrs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedCidr"))
  ; disallowed_cidrs =
      IpamPoolAllocationDisallowedCidrs.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DisallowedCidr"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
