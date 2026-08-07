open Aws.BaseTypes

type t =
  { subnet_identifier : String.t option
  ; subnet_availability_zone : AvailabilityZone.t option
  ; subnet_outpost : Outpost.t option
  ; subnet_status : String.t option
  }

let make ?subnet_identifier ?subnet_availability_zone ?subnet_outpost ?subnet_status () =
  { subnet_identifier; subnet_availability_zone; subnet_outpost; subnet_status }

let parse xml =
  Some
    { subnet_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SubnetIdentifier" xml) String.parse
    ; subnet_availability_zone =
        Aws.Util.option_bind
          (Aws.Xml.member "SubnetAvailabilityZone" xml)
          AvailabilityZone.parse
    ; subnet_outpost =
        Aws.Util.option_bind (Aws.Xml.member "SubnetOutpost" xml) Outpost.parse
    ; subnet_status =
        Aws.Util.option_bind (Aws.Xml.member "SubnetStatus" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_status (fun f ->
             Aws.Query.Pair ("SubnetStatus", String.to_query f))
       ; Aws.Util.option_map v.subnet_outpost (fun f ->
             Aws.Query.Pair ("SubnetOutpost", Outpost.to_query f))
       ; Aws.Util.option_map v.subnet_availability_zone (fun f ->
             Aws.Query.Pair ("SubnetAvailabilityZone", AvailabilityZone.to_query f))
       ; Aws.Util.option_map v.subnet_identifier (fun f ->
             Aws.Query.Pair ("SubnetIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subnet_status (fun f -> "SubnetStatus", String.to_json f)
       ; Aws.Util.option_map v.subnet_outpost (fun f ->
             "SubnetOutpost", Outpost.to_json f)
       ; Aws.Util.option_map v.subnet_availability_zone (fun f ->
             "SubnetAvailabilityZone", AvailabilityZone.to_json f)
       ; Aws.Util.option_map v.subnet_identifier (fun f ->
             "SubnetIdentifier", String.to_json f)
       ])

let of_json j =
  { subnet_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SubnetIdentifier") String.of_json
  ; subnet_availability_zone =
      Aws.Util.option_map
        (Aws.Json.lookup j "SubnetAvailabilityZone")
        AvailabilityZone.of_json
  ; subnet_outpost =
      Aws.Util.option_map (Aws.Json.lookup j "SubnetOutpost") Outpost.of_json
  ; subnet_status = Aws.Util.option_map (Aws.Json.lookup j "SubnetStatus") String.of_json
  }
