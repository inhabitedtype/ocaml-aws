open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; change_batch : ChangeBatch.t
  }

let make ~hosted_zone_id ~change_batch () = { hosted_zone_id; change_batch }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; change_batch =
        Aws.Xml.required
          "ChangeBatch"
          (Aws.Util.option_bind (Aws.Xml.member "ChangeBatch" xml) ChangeBatch.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ChangeBatch", ChangeBatch.to_query v.change_batch))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ChangeBatch", ChangeBatch.to_json v.change_batch)
       ; Some ("Id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; change_batch =
      ChangeBatch.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeBatch"))
  }
