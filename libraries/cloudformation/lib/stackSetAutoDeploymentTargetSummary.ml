open Aws.BaseTypes

type t =
  { organizational_unit_id : String.t option
  ; regions : RegionList.t
  }

let make ?organizational_unit_id ?(regions = []) () = { organizational_unit_id; regions }

let parse xml =
  Some
    { organizational_unit_id =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
    ; regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
       ; Aws.Util.option_map v.organizational_unit_id (fun f ->
             Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Regions", RegionList.to_json v.regions)
       ; Aws.Util.option_map v.organizational_unit_id (fun f ->
             "OrganizationalUnitId", String.to_json f)
       ])

let of_json j =
  { organizational_unit_id =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
  ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
  }
