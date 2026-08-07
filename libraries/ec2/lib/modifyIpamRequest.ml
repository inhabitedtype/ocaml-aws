open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_id : String.t
  ; description : String.t option
  ; add_operating_regions : AddIpamOperatingRegionSet.t
  ; remove_operating_regions : RemoveIpamOperatingRegionSet.t
  ; tier : IpamTier.t option
  ; enable_private_gua : Boolean.t option
  ; metered_account : IpamMeteredAccount.t option
  }

let make
    ?dry_run
    ~ipam_id
    ?description
    ?(add_operating_regions = [])
    ?(remove_operating_regions = [])
    ?tier
    ?enable_private_gua
    ?metered_account
    () =
  { dry_run
  ; ipam_id
  ; description
  ; add_operating_regions
  ; remove_operating_regions
  ; tier
  ; enable_private_gua
  ; metered_account
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_id =
        Aws.Xml.required
          "IpamId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamId" xml) String.parse)
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
    ; tier = Aws.Util.option_bind (Aws.Xml.member "Tier" xml) IpamTier.parse
    ; enable_private_gua =
        Aws.Util.option_bind (Aws.Xml.member "EnablePrivateGua" xml) Boolean.parse
    ; metered_account =
        Aws.Util.option_bind
          (Aws.Xml.member "MeteredAccount" xml)
          IpamMeteredAccount.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metered_account (fun f ->
             Aws.Query.Pair ("MeteredAccount", IpamMeteredAccount.to_query f))
       ; Aws.Util.option_map v.enable_private_gua (fun f ->
             Aws.Query.Pair ("EnablePrivateGua", Boolean.to_query f))
       ; Aws.Util.option_map v.tier (fun f ->
             Aws.Query.Pair ("Tier", IpamTier.to_query f))
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
       ; Some (Aws.Query.Pair ("IpamId", String.to_query v.ipam_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metered_account (fun f ->
             "MeteredAccount", IpamMeteredAccount.to_json f)
       ; Aws.Util.option_map v.enable_private_gua (fun f ->
             "EnablePrivateGua", Boolean.to_json f)
       ; Aws.Util.option_map v.tier (fun f -> "Tier", IpamTier.to_json f)
       ; Some
           ( "RemoveOperatingRegion"
           , RemoveIpamOperatingRegionSet.to_json v.remove_operating_regions )
       ; Some
           ( "AddOperatingRegion"
           , AddIpamOperatingRegionSet.to_json v.add_operating_regions )
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamId", String.to_json v.ipam_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; add_operating_regions =
      AddIpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddOperatingRegion"))
  ; remove_operating_regions =
      RemoveIpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveOperatingRegion"))
  ; tier = Aws.Util.option_map (Aws.Json.lookup j "Tier") IpamTier.of_json
  ; enable_private_gua =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePrivateGua") Boolean.of_json
  ; metered_account =
      Aws.Util.option_map (Aws.Json.lookup j "MeteredAccount") IpamMeteredAccount.of_json
  }
