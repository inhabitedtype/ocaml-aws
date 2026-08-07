open Aws.BaseTypes

type t =
  { host_id : String.t option
  ; mac_o_s_latest_supported_versions : MacOSVersionStringList.t
  }

let make ?host_id ?(mac_o_s_latest_supported_versions = []) () =
  { host_id; mac_o_s_latest_supported_versions }

let parse xml =
  Some
    { host_id = Aws.Util.option_bind (Aws.Xml.member "hostId" xml) String.parse
    ; mac_o_s_latest_supported_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "macOSLatestSupportedVersionSet" xml)
             MacOSVersionStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "MacOSLatestSupportedVersionSet"
              , MacOSVersionStringList.to_query v.mac_o_s_latest_supported_versions ))
       ; Aws.Util.option_map v.host_id (fun f ->
             Aws.Query.Pair ("HostId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "macOSLatestSupportedVersionSet"
           , MacOSVersionStringList.to_json v.mac_o_s_latest_supported_versions )
       ; Aws.Util.option_map v.host_id (fun f -> "hostId", String.to_json f)
       ])

let of_json j =
  { host_id = Aws.Util.option_map (Aws.Json.lookup j "hostId") String.of_json
  ; mac_o_s_latest_supported_versions =
      MacOSVersionStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "macOSLatestSupportedVersionSet"))
  }
