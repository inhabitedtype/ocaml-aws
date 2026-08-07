open Aws.BaseTypes

type t =
  { available_capacity : Integer.t option
  ; instance_type : String.t option
  ; total_capacity : Integer.t option
  }

let make ?available_capacity ?instance_type ?total_capacity () =
  { available_capacity; instance_type; total_capacity }

let parse xml =
  Some
    { available_capacity =
        Aws.Util.option_bind (Aws.Xml.member "availableCapacity" xml) Integer.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; total_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalCapacity" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_capacity (fun f ->
             Aws.Query.Pair ("TotalCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.available_capacity (fun f ->
             Aws.Query.Pair ("AvailableCapacity", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_capacity (fun f ->
             "totalCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.available_capacity (fun f ->
             "availableCapacity", Integer.to_json f)
       ])

let of_json j =
  { available_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "availableCapacity") Integer.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; total_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalCapacity") Integer.of_json
  }
