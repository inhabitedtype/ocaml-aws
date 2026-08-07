open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; role_arn : String.t
  ; feature_name : String.t
  }

let make ~d_b_instance_identifier ~role_arn ~feature_name () =
  { d_b_instance_identifier; role_arn; feature_name }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; role_arn =
        Aws.Xml.required
          "RoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "RoleArn" xml) String.parse)
    ; feature_name =
        Aws.Xml.required
          "FeatureName"
          (Aws.Util.option_bind (Aws.Xml.member "FeatureName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("FeatureName", String.to_query v.feature_name))
       ; Some (Aws.Query.Pair ("RoleArn", String.to_query v.role_arn))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("FeatureName", String.to_json v.feature_name)
       ; Some ("RoleArn", String.to_json v.role_arn)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; role_arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RoleArn"))
  ; feature_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FeatureName"))
  }
