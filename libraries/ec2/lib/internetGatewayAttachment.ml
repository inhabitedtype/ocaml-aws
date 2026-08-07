open Aws.BaseTypes

type t =
  { state : AttachmentStatus.t
  ; vpc_id : String.t
  }

let make ~state ~vpc_id () = { state; vpc_id }

let parse xml =
  Some
    { state =
        Aws.Xml.required
          "state"
          (Aws.Util.option_bind (Aws.Xml.member "state" xml) AttachmentStatus.parse)
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("State", AttachmentStatus.to_query v.state))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpcId", String.to_json v.vpc_id)
       ; Some ("state", AttachmentStatus.to_json v.state)
       ])

let of_json j =
  { state = AttachmentStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "state"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  }
