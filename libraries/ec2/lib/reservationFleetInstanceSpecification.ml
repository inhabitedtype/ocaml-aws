open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; instance_platform : CapacityReservationInstancePlatform.t option
  ; weight : Double.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; ebs_optimized : Boolean.t option
  ; priority : Integer.t option
  }

let make
    ?instance_type
    ?instance_platform
    ?weight
    ?availability_zone
    ?availability_zone_id
    ?ebs_optimized
    ?priority
    () =
  { instance_type
  ; instance_platform
  ; weight
  ; availability_zone
  ; availability_zone_id
  ; ebs_optimized
  ; priority
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceType.parse
    ; instance_platform =
        Aws.Util.option_bind
          (Aws.Xml.member "InstancePlatform" xml)
          CapacityReservationInstancePlatform.parse
    ; weight = Aws.Util.option_bind (Aws.Xml.member "Weight" xml) Double.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
    ; priority = Aws.Util.option_bind (Aws.Xml.member "Priority" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.priority (fun f ->
             Aws.Query.Pair ("Priority", Integer.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.weight (fun f ->
             Aws.Query.Pair ("Weight", Double.to_query f))
       ; Aws.Util.option_map v.instance_platform (fun f ->
             Aws.Query.Pair
               ("InstancePlatform", CapacityReservationInstancePlatform.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.priority (fun f -> "Priority", Integer.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "EbsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.weight (fun f -> "Weight", Double.to_json f)
       ; Aws.Util.option_map v.instance_platform (fun f ->
             "InstancePlatform", CapacityReservationInstancePlatform.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "InstanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceType") InstanceType.of_json
  ; instance_platform =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstancePlatform")
        CapacityReservationInstancePlatform.of_json
  ; weight = Aws.Util.option_map (Aws.Json.lookup j "Weight") Double.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
  ; priority = Aws.Util.option_map (Aws.Json.lookup j "Priority") Integer.of_json
  }
