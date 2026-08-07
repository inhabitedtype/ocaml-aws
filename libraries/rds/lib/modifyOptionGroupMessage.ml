open Aws.BaseTypes

type t =
  { option_group_name : String.t
  ; options_to_include : OptionConfigurationList.t
  ; options_to_remove : OptionNamesList.t
  ; apply_immediately : Boolean.t option
  }

let make
    ~option_group_name
    ?(options_to_include = [])
    ?(options_to_remove = [])
    ?apply_immediately
    () =
  { option_group_name; options_to_include; options_to_remove; apply_immediately }

let parse xml =
  Some
    { option_group_name =
        Aws.Xml.required
          "OptionGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse)
    ; options_to_include =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionsToInclude" xml)
             OptionConfigurationList.parse)
    ; options_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionsToRemove" xml)
             OptionNamesList.parse)
    ; apply_immediately =
        Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_immediately (fun f ->
             Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OptionsToRemove.member", OptionNamesList.to_query v.options_to_remove))
       ; Some
           (Aws.Query.Pair
              ( "OptionsToInclude.member"
              , OptionConfigurationList.to_query v.options_to_include ))
       ; Some (Aws.Query.Pair ("OptionGroupName", String.to_query v.option_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.apply_immediately (fun f ->
             "ApplyImmediately", Boolean.to_json f)
       ; Some ("OptionsToRemove", OptionNamesList.to_json v.options_to_remove)
       ; Some ("OptionsToInclude", OptionConfigurationList.to_json v.options_to_include)
       ; Some ("OptionGroupName", String.to_json v.option_group_name)
       ])

let of_json j =
  { option_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupName"))
  ; options_to_include =
      OptionConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionsToInclude"))
  ; options_to_remove =
      OptionNamesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionsToRemove"))
  ; apply_immediately =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
  }
