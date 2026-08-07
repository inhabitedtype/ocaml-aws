open Aws.BaseTypes

type t =
  { instance_type : String.t option
  ; availability_zone : String.t option
  ; tenancy : CapacityReservationTenancy.t option
  ; availability_zone_id : String.t option
  }

let make ?instance_type ?availability_zone ?tenancy ?availability_zone_id () =
  { instance_type; availability_zone; tenancy; availability_zone_id }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "tenancy" xml)
          CapacityReservationTenancy.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "tenancy", CapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ])

let of_json j =
  { instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "tenancy") CapacityReservationTenancy.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
