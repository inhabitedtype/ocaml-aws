open Aws.BaseTypes

type t =
  { accept_terms_and_conditions : Boolean.t option
  ; connection_arn : String.t option
  }

let make ?accept_terms_and_conditions ?connection_arn () =
  { accept_terms_and_conditions; connection_arn }

let parse xml =
  Some
    { accept_terms_and_conditions =
        Aws.Util.option_bind (Aws.Xml.member "AcceptTermsAndConditions" xml) Boolean.parse
    ; connection_arn =
        Aws.Util.option_bind (Aws.Xml.member "ConnectionArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.connection_arn (fun f ->
             Aws.Query.Pair ("ConnectionArn", String.to_query f))
       ; Aws.Util.option_map v.accept_terms_and_conditions (fun f ->
             Aws.Query.Pair ("AcceptTermsAndConditions", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.connection_arn (fun f -> "ConnectionArn", String.to_json f)
       ; Aws.Util.option_map v.accept_terms_and_conditions (fun f ->
             "AcceptTermsAndConditions", Boolean.to_json f)
       ])

let of_json j =
  { accept_terms_and_conditions =
      Aws.Util.option_map (Aws.Json.lookup j "AcceptTermsAndConditions") Boolean.of_json
  ; connection_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ConnectionArn") String.of_json
  }
