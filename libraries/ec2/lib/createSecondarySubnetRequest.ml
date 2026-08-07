open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; dry_run : Boolean.t option
  ; ipv4_cidr_block : String.t
  ; secondary_network_id : String.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_token
    ?availability_zone
    ?availability_zone_id
    ?dry_run
    ~ipv4_cidr_block
    ~secondary_network_id
    ?(tag_specifications = [])
    () =
  { client_token
  ; availability_zone
  ; availability_zone_id
  ; dry_run
  ; ipv4_cidr_block
  ; secondary_network_id
  ; tag_specifications
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipv4_cidr_block =
        Aws.Xml.required
          "Ipv4CidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "Ipv4CidrBlock" xml) String.parse)
    ; secondary_network_id =
        Aws.Xml.required
          "SecondaryNetworkId"
          (Aws.Util.option_bind (Aws.Xml.member "SecondaryNetworkId" xml) String.parse)
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
           (Aws.Query.Pair ("SecondaryNetworkId", String.to_query v.secondary_network_id))
       ; Some (Aws.Query.Pair ("Ipv4CidrBlock", String.to_query v.ipv4_cidr_block))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("SecondaryNetworkId", String.to_json v.secondary_network_id)
       ; Some ("Ipv4CidrBlock", String.to_json v.ipv4_cidr_block)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipv4_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv4CidrBlock"))
  ; secondary_network_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryNetworkId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
