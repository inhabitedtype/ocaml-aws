open Aws.BaseTypes

type t =
  { resource_arn : String.t
  ; apply_immediately : Boolean.t option
  }

let make ~resource_arn ?apply_immediately () = { resource_arn; apply_immediately }

let parse xml =
  Some
    { resource_arn =
        Aws.Xml.required
          "ResourceArn"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse)
    ; apply_immediately =
        Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_immediately (fun f ->
             Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ResourceArn", String.to_query v.resource_arn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_immediately (fun f ->
             "ApplyImmediately", Boolean.to_json f)
       ; Some ("ResourceArn", String.to_json v.resource_arn)
       ])

let of_json j =
  { resource_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceArn"))
  ; apply_immediately =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
  }
