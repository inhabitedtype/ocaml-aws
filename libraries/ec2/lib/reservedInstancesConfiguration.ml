open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; instance_count : Integer.t option
  ; instance_type : InstanceType.t option
  ; platform : String.t option
  ; scope : Scope.t option
  ; availability_zone_id : String.t option
  }

let make
    ?availability_zone
    ?instance_count
    ?instance_type
    ?platform
    ?scope
    ?availability_zone_id
    () =
  { availability_zone
  ; instance_count
  ; instance_type
  ; platform
  ; scope
  ; availability_zone_id
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) String.parse
    ; scope = Aws.Util.option_bind (Aws.Xml.member "scope" xml) Scope.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.scope (fun f -> Aws.Query.Pair ("Scope", Scope.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.scope (fun f -> "scope", Scope.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "platform", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") String.of_json
  ; scope = Aws.Util.option_map (Aws.Json.lookup j "scope") Scope.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  }
