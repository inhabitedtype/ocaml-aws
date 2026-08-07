open Aws.BaseTypes

type t =
  { policy_type_name : String.t option
  ; description : String.t option
  ; policy_attribute_type_descriptions : PolicyAttributeTypeDescriptions.t
  }

let make ?policy_type_name ?description ?(policy_attribute_type_descriptions = []) () =
  { policy_type_name; description; policy_attribute_type_descriptions }

let parse xml =
  Some
    { policy_type_name =
        Aws.Util.option_bind (Aws.Xml.member "PolicyTypeName" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; policy_attribute_type_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PolicyAttributeTypeDescriptions" xml)
             PolicyAttributeTypeDescriptions.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PolicyAttributeTypeDescriptions.member"
              , PolicyAttributeTypeDescriptions.to_query
                  v.policy_attribute_type_descriptions ))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.policy_type_name (fun f ->
             Aws.Query.Pair ("PolicyTypeName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "PolicyAttributeTypeDescriptions"
           , PolicyAttributeTypeDescriptions.to_json v.policy_attribute_type_descriptions
           )
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.policy_type_name (fun f ->
             "PolicyTypeName", String.to_json f)
       ])

let of_json j =
  { policy_type_name =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyTypeName") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; policy_attribute_type_descriptions =
      PolicyAttributeTypeDescriptions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyAttributeTypeDescriptions"))
  }
