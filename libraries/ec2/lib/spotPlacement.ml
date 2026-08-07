open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; group_name : String.t option
  ; tenancy : Tenancy.t option
  ; availability_zone_id : String.t option
  }

let make ?availability_zone ?group_name ?tenancy ?availability_zone_id () =
  { availability_zone; group_name; tenancy; availability_zone_id }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; tenancy = Aws.Util.option_bind (Aws.Xml.member "tenancy" xml) Tenancy.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f -> "tenancy", Tenancy.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; tenancy = Aws.Util.option_map (Aws.Json.lookup j "tenancy") Tenancy.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
