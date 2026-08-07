open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; requested_capacity : Integer.t
  ; client_token : String.t
  ; availability_zones : AvailabilityZonesLimit1.t
  ; availability_zone_ids : AvailabilityZoneIdsLimit1.t
  ; subnet_ids : SubnetIdsLimit1.t
  ; retry_strategy : RetryStrategy.t option
  }

let make
    ~auto_scaling_group_name
    ~requested_capacity
    ~client_token
    ?(availability_zones = [])
    ?(availability_zone_ids = [])
    ?(subnet_ids = [])
    ?retry_strategy
    () =
  { auto_scaling_group_name
  ; requested_capacity
  ; client_token
  ; availability_zones
  ; availability_zone_ids
  ; subnet_ids
  ; retry_strategy
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; requested_capacity =
        Aws.Xml.required
          "RequestedCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "RequestedCapacity" xml) Integer.parse)
    ; client_token =
        Aws.Xml.required
          "ClientToken"
          (Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse)
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZonesLimit1.parse)
    ; availability_zone_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZoneIds" xml)
             AvailabilityZoneIdsLimit1.parse)
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SubnetIds" xml) SubnetIdsLimit1.parse)
    ; retry_strategy =
        Aws.Util.option_bind (Aws.Xml.member "RetryStrategy" xml) RetryStrategy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retry_strategy (fun f ->
             Aws.Query.Pair ("RetryStrategy", RetryStrategy.to_query f))
       ; Some (Aws.Query.Pair ("SubnetIds.member", SubnetIdsLimit1.to_query v.subnet_ids))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZoneIds.member"
              , AvailabilityZoneIdsLimit1.to_query v.availability_zone_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZones.member"
              , AvailabilityZonesLimit1.to_query v.availability_zones ))
       ; Some (Aws.Query.Pair ("ClientToken", String.to_query v.client_token))
       ; Some
           (Aws.Query.Pair ("RequestedCapacity", Integer.to_query v.requested_capacity))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retry_strategy (fun f ->
             "RetryStrategy", RetryStrategy.to_json f)
       ; Some ("SubnetIds", SubnetIdsLimit1.to_json v.subnet_ids)
       ; Some
           ( "AvailabilityZoneIds"
           , AvailabilityZoneIdsLimit1.to_json v.availability_zone_ids )
       ; Some ("AvailabilityZones", AvailabilityZonesLimit1.to_json v.availability_zones)
       ; Some ("ClientToken", String.to_json v.client_token)
       ; Some ("RequestedCapacity", Integer.to_json v.requested_capacity)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; requested_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RequestedCapacity"))
  ; client_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientToken"))
  ; availability_zones =
      AvailabilityZonesLimit1.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; availability_zone_ids =
      AvailabilityZoneIdsLimit1.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZoneIds"))
  ; subnet_ids =
      SubnetIdsLimit1.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; retry_strategy =
      Aws.Util.option_map (Aws.Json.lookup j "RetryStrategy") RetryStrategy.of_json
  }
