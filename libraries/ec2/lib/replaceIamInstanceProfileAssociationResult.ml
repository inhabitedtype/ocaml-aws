type t = { iam_instance_profile_association : IamInstanceProfileAssociation.t option }

let make ?iam_instance_profile_association () = { iam_instance_profile_association }

let parse xml =
  Some
    { iam_instance_profile_association =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfileAssociation" xml)
          IamInstanceProfileAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.iam_instance_profile_association (fun f ->
             Aws.Query.Pair
               ("IamInstanceProfileAssociation", IamInstanceProfileAssociation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.iam_instance_profile_association (fun f ->
             "iamInstanceProfileAssociation", IamInstanceProfileAssociation.to_json f)
       ])

let of_json j =
  { iam_instance_profile_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfileAssociation")
        IamInstanceProfileAssociation.of_json
  }
