open Aws.BaseTypes

type t =
  { launch_templates : LaunchTemplateSet.t
  ; next_token : String.t option
  }

let make ?(launch_templates = []) ?next_token () = { launch_templates; next_token }

let parse xml =
  Some
    { launch_templates =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchTemplates" xml)
             LaunchTemplateSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LaunchTemplates", LaunchTemplateSet.to_query v.launch_templates))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("launchTemplates", LaunchTemplateSet.to_json v.launch_templates)
       ])

let of_json j =
  { launch_templates =
      LaunchTemplateSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchTemplates"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
