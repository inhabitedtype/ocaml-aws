type t = { regions : RegionList.t }

let make ?(regions = []) () = { regions }

let parse xml =
  Some
    { regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "regionInfo" xml) RegionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("RegionInfo", RegionList.to_query v.regions)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("regionInfo", RegionList.to_json v.regions) ])

let of_json j =
  { regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "regionInfo"))
  }
