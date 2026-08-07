open Aws.BaseTypes

type t =
  { d_b_parameter_group_name : String.t
  ; d_b_parameter_group_family : String.t
  ; description : String.t
  ; tags : TagList.t
  }

let make
    ~d_b_parameter_group_name
    ~d_b_parameter_group_family
    ~description
    ?(tags = [])
    () =
  { d_b_parameter_group_name; d_b_parameter_group_family; description; tags }

let parse xml =
  Some
    { d_b_parameter_group_name =
        Aws.Xml.required
          "DBParameterGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupName" xml) String.parse)
    ; d_b_parameter_group_family =
        Aws.Xml.required
          "DBParameterGroupFamily"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBParameterGroupFamily" xml)
             String.parse)
    ; description =
        Aws.Xml.required
          "Description"
          (Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("Description", String.to_query v.description))
       ; Some
           (Aws.Query.Pair
              ("DBParameterGroupFamily", String.to_query v.d_b_parameter_group_family))
       ; Some
           (Aws.Query.Pair
              ("DBParameterGroupName", String.to_query v.d_b_parameter_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("Description", String.to_json v.description)
       ; Some ("DBParameterGroupFamily", String.to_json v.d_b_parameter_group_family)
       ; Some ("DBParameterGroupName", String.to_json v.d_b_parameter_group_name)
       ])

let of_json j =
  { d_b_parameter_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBParameterGroupName"))
  ; d_b_parameter_group_family =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBParameterGroupFamily"))
  ; description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Description"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
