open Aws.BaseTypes

type t =
  { opt_in_status : String.t option
  ; geography : RegionGeographyList.t
  ; region_name : String.t option
  ; endpoint : String.t option
  }

let make ?opt_in_status ?(geography = []) ?region_name ?endpoint () =
  { opt_in_status; geography; region_name; endpoint }

let parse xml =
  Some
    { opt_in_status = Aws.Util.option_bind (Aws.Xml.member "optInStatus" xml) String.parse
    ; geography =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "geographySet" xml)
             RegionGeographyList.parse)
    ; region_name = Aws.Util.option_bind (Aws.Xml.member "regionName" xml) String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "regionEndpoint" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("RegionEndpoint", String.to_query f))
       ; Aws.Util.option_map v.region_name (fun f ->
             Aws.Query.Pair ("RegionName", String.to_query f))
       ; Some (Aws.Query.Pair ("GeographySet", RegionGeographyList.to_query v.geography))
       ; Aws.Util.option_map v.opt_in_status (fun f ->
             Aws.Query.Pair ("OptInStatus", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.endpoint (fun f -> "regionEndpoint", String.to_json f)
       ; Aws.Util.option_map v.region_name (fun f -> "regionName", String.to_json f)
       ; Some ("geographySet", RegionGeographyList.to_json v.geography)
       ; Aws.Util.option_map v.opt_in_status (fun f -> "optInStatus", String.to_json f)
       ])

let of_json j =
  { opt_in_status = Aws.Util.option_map (Aws.Json.lookup j "optInStatus") String.of_json
  ; geography =
      RegionGeographyList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "geographySet"))
  ; region_name = Aws.Util.option_map (Aws.Json.lookup j "regionName") String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "regionEndpoint") String.of_json
  }
