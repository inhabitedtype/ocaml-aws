open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; availability_zone_id : String.t option
  ; instance_type : InstanceType.t option
  ; instance_platform : CapacityReservationInstancePlatform.t option
  ; availability_zone : String.t option
  ; total_instance_count : Integer.t option
  ; fulfilled_capacity : Double.t option
  ; ebs_optimized : Boolean.t option
  ; create_date : DateTime.t option
  ; weight : Double.t option
  ; priority : Integer.t option
  }

let make
    ?capacity_reservation_id
    ?availability_zone_id
    ?instance_type
    ?instance_platform
    ?availability_zone
    ?total_instance_count
    ?fulfilled_capacity
    ?ebs_optimized
    ?create_date
    ?weight
    ?priority
    () =
  { capacity_reservation_id
  ; availability_zone_id
  ; instance_type
  ; instance_platform
  ; availability_zone
  ; total_instance_count
  ; fulfilled_capacity
  ; ebs_optimized
  ; create_date
  ; weight
  ; priority
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; instance_platform =
        Aws.Util.option_bind
          (Aws.Xml.member "instancePlatform" xml)
          CapacityReservationInstancePlatform.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; total_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "totalInstanceCount" xml) Integer.parse
    ; fulfilled_capacity =
        Aws.Util.option_bind (Aws.Xml.member "fulfilledCapacity" xml) Double.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; weight = Aws.Util.option_bind (Aws.Xml.member "weight" xml) Double.parse
    ; priority = Aws.Util.option_bind (Aws.Xml.member "priority" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.priority (fun f ->
             Aws.Query.Pair ("Priority", Integer.to_query f))
       ; Aws.Util.option_map v.weight (fun f ->
             Aws.Query.Pair ("Weight", Double.to_query f))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             Aws.Query.Pair ("FulfilledCapacity", Double.to_query f))
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             Aws.Query.Pair ("TotalInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_platform (fun f ->
             Aws.Query.Pair
               ("InstancePlatform", CapacityReservationInstancePlatform.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.priority (fun f -> "priority", Integer.to_json f)
       ; Aws.Util.option_map v.weight (fun f -> "weight", Double.to_json f)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             "fulfilledCapacity", Double.to_json f)
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             "totalInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_platform (fun f ->
             "instancePlatform", CapacityReservationInstancePlatform.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; instance_platform =
      Aws.Util.option_map
        (Aws.Json.lookup j "instancePlatform")
        CapacityReservationInstancePlatform.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; total_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "totalInstanceCount") Integer.of_json
  ; fulfilled_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "fulfilledCapacity") Double.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; weight = Aws.Util.option_map (Aws.Json.lookup j "weight") Double.of_json
  ; priority = Aws.Util.option_map (Aws.Json.lookup j "priority") Integer.of_json
  }
