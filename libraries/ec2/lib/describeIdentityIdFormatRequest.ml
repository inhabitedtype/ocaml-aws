open Aws.BaseTypes

type t =
  { resource : String.t option
  ; principal_arn : String.t
  }

let make ?resource ~principal_arn () = { resource; principal_arn }

let parse xml =
  Some
    { resource = Aws.Util.option_bind (Aws.Xml.member "resource" xml) String.parse
    ; principal_arn =
        Aws.Xml.required
          "principalArn"
          (Aws.Util.option_bind (Aws.Xml.member "principalArn" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("PrincipalArn", String.to_query v.principal_arn))
       ; Aws.Util.option_map v.resource (fun f ->
             Aws.Query.Pair ("Resource", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("principalArn", String.to_json v.principal_arn)
       ; Aws.Util.option_map v.resource (fun f -> "resource", String.to_json f)
       ])

let of_json j =
  { resource = Aws.Util.option_map (Aws.Json.lookup j "resource") String.of_json
  ; principal_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "principalArn"))
  }
