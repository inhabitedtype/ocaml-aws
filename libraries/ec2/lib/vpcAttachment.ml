open Aws.BaseTypes

type t =
  { vpc_id : String.t option
  ; state : AttachmentStatus.t option
  }

let make ?vpc_id ?state () = { vpc_id; state }

let parse xml =
  Some
    { vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) AttachmentStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", AttachmentStatus.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", AttachmentStatus.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ])

let of_json j =
  { vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") AttachmentStatus.of_json
  }
