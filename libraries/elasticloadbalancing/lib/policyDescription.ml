open Aws.BaseTypes

type t =
  { policy_name : String.t option
  ; policy_type_name : String.t option
  ; policy_attribute_descriptions : PolicyAttributeDescriptions.t
  }

let make ?policy_name ?policy_type_name ?(policy_attribute_descriptions = []) () =
  { policy_name; policy_type_name; policy_attribute_descriptions }

let parse xml =
  Some
    { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
    ; policy_type_name =
        Aws.Util.option_bind (Aws.Xml.member "PolicyTypeName" xml) String.parse
    ; policy_attribute_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyAttributeDescriptions" xml)
             PolicyAttributeDescriptions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PolicyAttributeDescriptions.member"
              , PolicyAttributeDescriptions.to_query v.policy_attribute_descriptions ))
       ; Aws.Util.option_map v.policy_type_name (fun f ->
             Aws.Query.Pair ("PolicyTypeName", String.to_query f))
       ; Aws.Util.option_map v.policy_name (fun f ->
             Aws.Query.Pair ("PolicyName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PolicyAttributeDescriptions"
           , PolicyAttributeDescriptions.to_json v.policy_attribute_descriptions )
       ; Aws.Util.option_map v.policy_type_name (fun f ->
             "PolicyTypeName", String.to_json f)
       ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
       ])

let of_json j =
  { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
  ; policy_type_name =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyTypeName") String.of_json
  ; policy_attribute_descriptions =
      PolicyAttributeDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyAttributeDescriptions"))
  }
