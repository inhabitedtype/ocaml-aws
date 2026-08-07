open Aws.BaseTypes

type t =
  { launch_template_versions : LaunchTemplateVersionSet.t
  ; next_token : String.t option
  }

let make ?(launch_template_versions = []) ?next_token () =
  { launch_template_versions; next_token }

let parse xml =
  Some
    { launch_template_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchTemplateVersionSet" xml)
             LaunchTemplateVersionSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateVersionSet"
              , LaunchTemplateVersionSet.to_query v.launch_template_versions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "launchTemplateVersionSet"
           , LaunchTemplateVersionSet.to_json v.launch_template_versions )
       ])

let of_json j =
  { launch_template_versions =
      LaunchTemplateVersionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchTemplateVersionSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
