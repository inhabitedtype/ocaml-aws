open Aws.BaseTypes

type t =
  { iam_instance_profile : IamInstanceProfileSpecification.t
  ; instance_id : String.t
  }

let make ~iam_instance_profile ~instance_id () = { iam_instance_profile; instance_id }

let parse xml =
  Some
    { iam_instance_profile =
        Aws.Xml.required
          "IamInstanceProfile"
          (Aws.Util.option_bind
             (Aws.Xml.member "IamInstanceProfile" xml)
             IamInstanceProfileSpecification.parse)
    ; instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Some
           (Aws.Query.Pair
              ( "IamInstanceProfile"
              , IamInstanceProfileSpecification.to_query v.iam_instance_profile ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("InstanceId", String.to_json v.instance_id)
       ; Some
           ( "IamInstanceProfile"
           , IamInstanceProfileSpecification.to_json v.iam_instance_profile )
       ])

let of_json j =
  { iam_instance_profile =
      IamInstanceProfileSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IamInstanceProfile"))
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  }
