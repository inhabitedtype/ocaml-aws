open Aws.BaseTypes

type t =
  { node_group_id : String.t option
  ; preferred_availability_zones : AvailabilityZonesList.t
  }

let make ?node_group_id ?(preferred_availability_zones = []) () =
  { node_group_id; preferred_availability_zones }

let parse xml =
  Some
    { node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
    ; preferred_availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredAvailabilityZones" xml)
             AvailabilityZonesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PreferredAvailabilityZones.member"
              , AvailabilityZonesList.to_query v.preferred_availability_zones ))
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PreferredAvailabilityZones"
           , AvailabilityZonesList.to_json v.preferred_availability_zones )
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ])

let of_json j =
  { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
  ; preferred_availability_zones =
      AvailabilityZonesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
  }
