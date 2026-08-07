open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; v_p_c : VPC.t
  ; comment : String.t option
  }

let make ~hosted_zone_id ~v_p_c ?comment () = { hosted_zone_id; v_p_c; comment }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; v_p_c =
        Aws.Xml.required "VPC" (Aws.Util.option_bind (Aws.Xml.member "VPC" xml) VPC.parse)
    ; comment = Aws.Util.option_bind (Aws.Xml.member "Comment" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f ->
             Aws.Query.Pair ("Comment", String.to_query f))
       ; Some (Aws.Query.Pair ("VPC", VPC.to_query v.v_p_c))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.comment (fun f -> "Comment", String.to_json f)
       ; Some ("VPC", VPC.to_json v.v_p_c)
       ; Some ("Id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; v_p_c = VPC.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VPC"))
  ; comment = Aws.Util.option_map (Aws.Json.lookup j "Comment") String.of_json
  }
