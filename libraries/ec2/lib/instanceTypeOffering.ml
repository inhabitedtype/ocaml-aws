open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; location_type : LocationType.t option
  ; location : String.t option
  }

let make ?instance_type ?location_type ?location () =
  { instance_type; location_type; location }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; location_type =
        Aws.Util.option_bind (Aws.Xml.member "locationType" xml) LocationType.parse
    ; location = Aws.Util.option_bind (Aws.Xml.member "location" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f ->
             Aws.Query.Pair ("Location", String.to_query f))
       ; Aws.Util.option_map v.location_type (fun f ->
             Aws.Query.Pair ("LocationType", LocationType.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.location (fun f -> "location", String.to_json f)
       ; Aws.Util.option_map v.location_type (fun f ->
             "locationType", LocationType.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; location_type =
      Aws.Util.option_map (Aws.Json.lookup j "locationType") LocationType.of_json
  ; location = Aws.Util.option_map (Aws.Json.lookup j "location") String.of_json
  }
