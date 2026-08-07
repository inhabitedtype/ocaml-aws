open Aws.BaseTypes

type t =
  { select_expression : String.t
  ; next_token : String.t option
  ; consistent_read : Boolean.t option
  }

let make ~select_expression ?next_token ?consistent_read () =
  { select_expression; next_token; consistent_read }

let parse xml =
  Some
    { select_expression =
        Aws.Xml.required
          "SelectExpression"
          (Aws.Util.option_bind (Aws.Xml.member "SelectExpression" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; consistent_read =
        Aws.Util.option_bind (Aws.Xml.member "ConsistentRead" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.consistent_read (fun f ->
             Aws.Query.Pair ("ConsistentRead", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("SelectExpression", String.to_query v.select_expression))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.consistent_read (fun f ->
             "ConsistentRead", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("SelectExpression", String.to_json v.select_expression)
       ])

let of_json j =
  { select_expression =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SelectExpression"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; consistent_read =
      Aws.Util.option_map (Aws.Json.lookup j "ConsistentRead") Boolean.of_json
  }
