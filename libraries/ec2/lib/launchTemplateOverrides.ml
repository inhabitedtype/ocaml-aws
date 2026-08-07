open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; spot_price : String.t option
  ; subnet_id : String.t option
  ; availability_zone : String.t option
  ; weighted_capacity : Double.t option
  ; priority : Double.t option
  ; instance_requirements : InstanceRequirements.t option
  ; availability_zone_id : String.t option
  }

let make
    ?instance_type
    ?spot_price
    ?subnet_id
    ?availability_zone
    ?weighted_capacity
    ?priority
    ?instance_requirements
    ?availability_zone_id
    () =
  { instance_type
  ; spot_price
  ; subnet_id
  ; availability_zone
  ; weighted_capacity
  ; priority
  ; instance_requirements
  ; availability_zone_id
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; weighted_capacity =
        Aws.Util.option_bind (Aws.Xml.member "weightedCapacity" xml) Double.parse
    ; priority = Aws.Util.option_bind (Aws.Xml.member "priority" xml) Double.parse
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceRequirements" xml)
          InstanceRequirements.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair ("InstanceRequirements", InstanceRequirements.to_query f))
       ; Aws.Util.option_map v.priority (fun f ->
             Aws.Query.Pair ("Priority", Double.to_query f))
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             Aws.Query.Pair ("WeightedCapacity", Double.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "instanceRequirements", InstanceRequirements.to_json f)
       ; Aws.Util.option_map v.priority (fun f -> "priority", Double.to_json f)
       ; Aws.Util.option_map v.weighted_capacity (fun f ->
             "weightedCapacity", Double.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; weighted_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "weightedCapacity") Double.of_json
  ; priority = Aws.Util.option_map (Aws.Json.lookup j "priority") Double.of_json
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceRequirements")
        InstanceRequirements.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
