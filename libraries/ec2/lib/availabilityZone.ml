open Aws.BaseTypes

type t =
  { opt_in_status : AvailabilityZoneOptInStatus.t option
  ; messages : AvailabilityZoneMessageList.t
  ; region_name : String.t option
  ; zone_name : String.t option
  ; zone_id : String.t option
  ; group_name : String.t option
  ; network_border_group : String.t option
  ; zone_type : String.t option
  ; parent_zone_name : String.t option
  ; parent_zone_id : String.t option
  ; group_long_name : String.t option
  ; geography : AvailabilityZoneGeographyList.t
  ; sub_geography : AvailabilityZoneSubGeographyList.t
  ; state : AvailabilityZoneState.t option
  }

let make
    ?opt_in_status
    ?(messages = [])
    ?region_name
    ?zone_name
    ?zone_id
    ?group_name
    ?network_border_group
    ?zone_type
    ?parent_zone_name
    ?parent_zone_id
    ?group_long_name
    ?(geography = [])
    ?(sub_geography = [])
    ?state
    () =
  { opt_in_status
  ; messages
  ; region_name
  ; zone_name
  ; zone_id
  ; group_name
  ; network_border_group
  ; zone_type
  ; parent_zone_name
  ; parent_zone_id
  ; group_long_name
  ; geography
  ; sub_geography
  ; state
  }

let parse xml =
  Some
    { opt_in_status =
        Aws.Util.option_bind
          (Aws.Xml.member "optInStatus" xml)
          AvailabilityZoneOptInStatus.parse
    ; messages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "messageSet" xml)
             AvailabilityZoneMessageList.parse)
    ; region_name = Aws.Util.option_bind (Aws.Xml.member "regionName" xml) String.parse
    ; zone_name = Aws.Util.option_bind (Aws.Xml.member "zoneName" xml) String.parse
    ; zone_id = Aws.Util.option_bind (Aws.Xml.member "zoneId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "networkBorderGroup" xml) String.parse
    ; zone_type = Aws.Util.option_bind (Aws.Xml.member "zoneType" xml) String.parse
    ; parent_zone_name =
        Aws.Util.option_bind (Aws.Xml.member "parentZoneName" xml) String.parse
    ; parent_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "parentZoneId" xml) String.parse
    ; group_long_name =
        Aws.Util.option_bind (Aws.Xml.member "groupLongName" xml) String.parse
    ; geography =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "geographySet" xml)
             AvailabilityZoneGeographyList.parse)
    ; sub_geography =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "subGeographySet" xml)
             AvailabilityZoneSubGeographyList.parse)
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "zoneState" xml) AvailabilityZoneState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("ZoneState", AvailabilityZoneState.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SubGeographySet"
              , AvailabilityZoneSubGeographyList.to_query v.sub_geography ))
       ; Some
           (Aws.Query.Pair
              ("GeographySet", AvailabilityZoneGeographyList.to_query v.geography))
       ; Aws.Util.option_map v.group_long_name (fun f ->
             Aws.Query.Pair ("GroupLongName", String.to_query f))
       ; Aws.Util.option_map v.parent_zone_id (fun f ->
             Aws.Query.Pair ("ParentZoneId", String.to_query f))
       ; Aws.Util.option_map v.parent_zone_name (fun f ->
             Aws.Query.Pair ("ParentZoneName", String.to_query f))
       ; Aws.Util.option_map v.zone_type (fun f ->
             Aws.Query.Pair ("ZoneType", String.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.zone_id (fun f ->
             Aws.Query.Pair ("ZoneId", String.to_query f))
       ; Aws.Util.option_map v.zone_name (fun f ->
             Aws.Query.Pair ("ZoneName", String.to_query f))
       ; Aws.Util.option_map v.region_name (fun f ->
             Aws.Query.Pair ("RegionName", String.to_query f))
       ; Some
           (Aws.Query.Pair ("MessageSet", AvailabilityZoneMessageList.to_query v.messages))
       ; Aws.Util.option_map v.opt_in_status (fun f ->
             Aws.Query.Pair ("OptInStatus", AvailabilityZoneOptInStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "zoneState", AvailabilityZoneState.to_json f)
       ; Some ("subGeographySet", AvailabilityZoneSubGeographyList.to_json v.sub_geography)
       ; Some ("geographySet", AvailabilityZoneGeographyList.to_json v.geography)
       ; Aws.Util.option_map v.group_long_name (fun f ->
             "groupLongName", String.to_json f)
       ; Aws.Util.option_map v.parent_zone_id (fun f -> "parentZoneId", String.to_json f)
       ; Aws.Util.option_map v.parent_zone_name (fun f ->
             "parentZoneName", String.to_json f)
       ; Aws.Util.option_map v.zone_type (fun f -> "zoneType", String.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "networkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.zone_id (fun f -> "zoneId", String.to_json f)
       ; Aws.Util.option_map v.zone_name (fun f -> "zoneName", String.to_json f)
       ; Aws.Util.option_map v.region_name (fun f -> "regionName", String.to_json f)
       ; Some ("messageSet", AvailabilityZoneMessageList.to_json v.messages)
       ; Aws.Util.option_map v.opt_in_status (fun f ->
             "optInStatus", AvailabilityZoneOptInStatus.to_json f)
       ])

let of_json j =
  { opt_in_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "optInStatus")
        AvailabilityZoneOptInStatus.of_json
  ; messages =
      AvailabilityZoneMessageList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "messageSet"))
  ; region_name = Aws.Util.option_map (Aws.Json.lookup j "regionName") String.of_json
  ; zone_name = Aws.Util.option_map (Aws.Json.lookup j "zoneName") String.of_json
  ; zone_id = Aws.Util.option_map (Aws.Json.lookup j "zoneId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "networkBorderGroup") String.of_json
  ; zone_type = Aws.Util.option_map (Aws.Json.lookup j "zoneType") String.of_json
  ; parent_zone_name =
      Aws.Util.option_map (Aws.Json.lookup j "parentZoneName") String.of_json
  ; parent_zone_id = Aws.Util.option_map (Aws.Json.lookup j "parentZoneId") String.of_json
  ; group_long_name =
      Aws.Util.option_map (Aws.Json.lookup j "groupLongName") String.of_json
  ; geography =
      AvailabilityZoneGeographyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "geographySet"))
  ; sub_geography =
      AvailabilityZoneSubGeographyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "subGeographySet"))
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "zoneState") AvailabilityZoneState.of_json
  }
