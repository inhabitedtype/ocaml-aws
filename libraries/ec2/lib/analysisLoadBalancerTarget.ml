open Aws.BaseTypes

type t =
  { address : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; instance : AnalysisComponent.t option
  ; port : Integer.t option
  }

let make ?address ?availability_zone ?availability_zone_id ?instance ?port () =
  { address; availability_zone; availability_zone_id; instance; port }

let parse xml =
  Some
    { address = Aws.Util.option_bind (Aws.Xml.member "address" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; instance =
        Aws.Util.option_bind (Aws.Xml.member "instance" xml) AnalysisComponent.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "port" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.instance (fun f ->
             Aws.Query.Pair ("Instance", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.port (fun f -> "port", Integer.to_json f)
       ; Aws.Util.option_map v.instance (fun f -> "instance", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.address (fun f -> "address", String.to_json f)
       ])

let of_json j =
  { address = Aws.Util.option_map (Aws.Json.lookup j "address") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; instance =
      Aws.Util.option_map (Aws.Json.lookup j "instance") AnalysisComponent.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "port") Integer.of_json
  }
