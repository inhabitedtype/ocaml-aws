open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_resource_discovery_id : String.t
  ; description : String.t option
  ; add_operating_regions : AddIpamOperatingRegionSet.t
  ; remove_operating_regions : RemoveIpamOperatingRegionSet.t
  ; add_organizational_unit_exclusions : AddIpamOrganizationalUnitExclusionSet.t
  ; remove_organizational_unit_exclusions : RemoveIpamOrganizationalUnitExclusionSet.t
  }

let make
    ?dry_run
    ~ipam_resource_discovery_id
    ?description
    ?(add_operating_regions = [])
    ?(remove_operating_regions = [])
    ?(add_organizational_unit_exclusions = [])
    ?(remove_organizational_unit_exclusions = [])
    () =
  { dry_run
  ; ipam_resource_discovery_id
  ; description
  ; add_operating_regions
  ; remove_operating_regions
  ; add_organizational_unit_exclusions
  ; remove_organizational_unit_exclusions
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_resource_discovery_id =
        Aws.Xml.required
          "IpamResourceDiscoveryId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamResourceDiscoveryId" xml)
             String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; add_operating_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddOperatingRegion" xml)
             AddIpamOperatingRegionSet.parse)
    ; remove_operating_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveOperatingRegion" xml)
             RemoveIpamOperatingRegionSet.parse)
    ; add_organizational_unit_exclusions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddOrganizationalUnitExclusion" xml)
             AddIpamOrganizationalUnitExclusionSet.parse)
    ; remove_organizational_unit_exclusions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveOrganizationalUnitExclusion" xml)
             RemoveIpamOrganizationalUnitExclusionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RemoveOrganizationalUnitExclusion"
              , RemoveIpamOrganizationalUnitExclusionSet.to_query
                  v.remove_organizational_unit_exclusions ))
       ; Some
           (Aws.Query.Pair
              ( "AddOrganizationalUnitExclusion"
              , AddIpamOrganizationalUnitExclusionSet.to_query
                  v.add_organizational_unit_exclusions ))
       ; Some
           (Aws.Query.Pair
              ( "RemoveOperatingRegion"
              , RemoveIpamOperatingRegionSet.to_query v.remove_operating_regions ))
       ; Some
           (Aws.Query.Pair
              ( "AddOperatingRegion"
              , AddIpamOperatingRegionSet.to_query v.add_operating_regions ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("IpamResourceDiscoveryId", String.to_query v.ipam_resource_discovery_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "RemoveOrganizationalUnitExclusion"
           , RemoveIpamOrganizationalUnitExclusionSet.to_json
               v.remove_organizational_unit_exclusions )
       ; Some
           ( "AddOrganizationalUnitExclusion"
           , AddIpamOrganizationalUnitExclusionSet.to_json
               v.add_organizational_unit_exclusions )
       ; Some
           ( "RemoveOperatingRegion"
           , RemoveIpamOperatingRegionSet.to_json v.remove_operating_regions )
       ; Some
           ( "AddOperatingRegion"
           , AddIpamOperatingRegionSet.to_json v.add_operating_regions )
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamResourceDiscoveryId", String.to_json v.ipam_resource_discovery_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_resource_discovery_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamResourceDiscoveryId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; add_operating_regions =
      AddIpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddOperatingRegion"))
  ; remove_operating_regions =
      RemoveIpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveOperatingRegion"))
  ; add_organizational_unit_exclusions =
      AddIpamOrganizationalUnitExclusionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddOrganizationalUnitExclusion"))
  ; remove_organizational_unit_exclusions =
      RemoveIpamOrganizationalUnitExclusionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveOrganizationalUnitExclusion"))
  }
