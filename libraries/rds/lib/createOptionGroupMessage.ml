open Aws.BaseTypes

type t =
  { option_group_name : String.t
  ; engine_name : String.t
  ; major_engine_version : String.t
  ; option_group_description : String.t
  ; tags : TagList.t
  }

let make
    ~option_group_name
    ~engine_name
    ~major_engine_version
    ~option_group_description
    ?(tags = [])
    () =
  { option_group_name; engine_name; major_engine_version; option_group_description; tags }

let parse xml =
  Some
    { option_group_name =
        Aws.Xml.required
          "OptionGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse)
    ; engine_name =
        Aws.Xml.required
          "EngineName"
          (Aws.Util.option_bind (Aws.Xml.member "EngineName" xml) String.parse)
    ; major_engine_version =
        Aws.Xml.required
          "MajorEngineVersion"
          (Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse)
    ; option_group_description =
        Aws.Xml.required
          "OptionGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupDescription" xml)
             String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("OptionGroupDescription", String.to_query v.option_group_description))
       ; Some
           (Aws.Query.Pair ("MajorEngineVersion", String.to_query v.major_engine_version))
       ; Some (Aws.Query.Pair ("EngineName", String.to_query v.engine_name))
       ; Some (Aws.Query.Pair ("OptionGroupName", String.to_query v.option_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("OptionGroupDescription", String.to_json v.option_group_description)
       ; Some ("MajorEngineVersion", String.to_json v.major_engine_version)
       ; Some ("EngineName", String.to_json v.engine_name)
       ; Some ("OptionGroupName", String.to_json v.option_group_name)
       ])

let of_json j =
  { option_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupName"))
  ; engine_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineName"))
  ; major_engine_version =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MajorEngineVersion"))
  ; option_group_description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupDescription"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
