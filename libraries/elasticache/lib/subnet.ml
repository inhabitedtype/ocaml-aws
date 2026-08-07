open Aws.BaseTypes

type t =
  { subnet_identifier : String.t option
  ; subnet_availability_zone : AvailabilityZone.t option
  ; subnet_outpost : SubnetOutpost.t option
  ; supported_network_types : NetworkTypeList.t
  }

let make
    ?subnet_identifier
    ?subnet_availability_zone
    ?subnet_outpost
    ?(supported_network_types = [])
    () =
  { subnet_identifier; subnet_availability_zone; subnet_outpost; supported_network_types }

let parse xml =
  Some
    { subnet_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SubnetIdentifier" xml) String.parse
    ; subnet_availability_zone =
        Aws.Util.option_bind
          (Aws.Xml.member "SubnetAvailabilityZone" xml)
          AvailabilityZone.parse
    ; subnet_outpost =
        Aws.Util.option_bind (Aws.Xml.member "SubnetOutpost" xml) SubnetOutpost.parse
    ; supported_network_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedNetworkTypes" xml)
             NetworkTypeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SupportedNetworkTypes.member"
              , NetworkTypeList.to_query v.supported_network_types ))
       ; Aws.Util.option_map v.subnet_outpost (fun f ->
             Aws.Query.Pair ("SubnetOutpost", SubnetOutpost.to_query f))
       ; Aws.Util.option_map v.subnet_availability_zone (fun f ->
             Aws.Query.Pair ("SubnetAvailabilityZone", AvailabilityZone.to_query f))
       ; Aws.Util.option_map v.subnet_identifier (fun f ->
             Aws.Query.Pair ("SubnetIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SupportedNetworkTypes", NetworkTypeList.to_json v.supported_network_types)
       ; Aws.Util.option_map v.subnet_outpost (fun f ->
             "SubnetOutpost", SubnetOutpost.to_json f)
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
      Aws.Util.option_map (Aws.Json.lookup j "SubnetOutpost") SubnetOutpost.of_json
  ; supported_network_types =
      NetworkTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedNetworkTypes"))
  }
