type t = { trail_list : TrailList.t }

let make ?(trail_list = []) () = { trail_list }

let parse xml =
  Some
    { trail_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "trailList" xml) TrailList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("trailList.member", TrailList.to_query v.trail_list)) ])

let to_json v =
  `Assoc (Aws.Util.list_filter_opt [ Some ("trailList", TrailList.to_json v.trail_list) ])

let of_json j =
  { trail_list =
      TrailList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "trailList"))
  }
