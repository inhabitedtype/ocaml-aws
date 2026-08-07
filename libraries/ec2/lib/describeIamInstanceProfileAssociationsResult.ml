open Aws.BaseTypes

type t =
  { iam_instance_profile_associations : IamInstanceProfileAssociationSet.t
  ; next_token : String.t option
  }

let make ?(iam_instance_profile_associations = []) ?next_token () =
  { iam_instance_profile_associations; next_token }

let parse xml =
  Some
    { iam_instance_profile_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "iamInstanceProfileAssociationSet" xml)
             IamInstanceProfileAssociationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "IamInstanceProfileAssociationSet"
              , IamInstanceProfileAssociationSet.to_query
                  v.iam_instance_profile_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "iamInstanceProfileAssociationSet"
           , IamInstanceProfileAssociationSet.to_json v.iam_instance_profile_associations
           )
       ])

let of_json j =
  { iam_instance_profile_associations =
      IamInstanceProfileAssociationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "iamInstanceProfileAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
