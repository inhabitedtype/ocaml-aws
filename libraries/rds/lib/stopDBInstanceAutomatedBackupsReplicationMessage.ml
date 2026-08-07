open Aws.BaseTypes

type t = { source_d_b_instance_arn : String.t }

let make ~source_d_b_instance_arn () = { source_d_b_instance_arn }

let parse xml =
  Some
    { source_d_b_instance_arn =
        Aws.Xml.required
          "SourceDBInstanceArn"
          (Aws.Util.option_bind (Aws.Xml.member "SourceDBInstanceArn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("SourceDBInstanceArn", String.to_query v.source_d_b_instance_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SourceDBInstanceArn", String.to_json v.source_d_b_instance_arn) ])

let of_json j =
  { source_d_b_instance_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceDBInstanceArn"))
  }
