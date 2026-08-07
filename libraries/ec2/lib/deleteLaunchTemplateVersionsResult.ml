type t =
  { successfully_deleted_launch_template_versions :
      DeleteLaunchTemplateVersionsResponseSuccessSet.t
  ; unsuccessfully_deleted_launch_template_versions :
      DeleteLaunchTemplateVersionsResponseErrorSet.t
  }

let make
    ?(successfully_deleted_launch_template_versions = [])
    ?(unsuccessfully_deleted_launch_template_versions = [])
    () =
  { successfully_deleted_launch_template_versions
  ; unsuccessfully_deleted_launch_template_versions
  }

let parse xml =
  Some
    { successfully_deleted_launch_template_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successfullyDeletedLaunchTemplateVersionSet" xml)
             DeleteLaunchTemplateVersionsResponseSuccessSet.parse)
    ; unsuccessfully_deleted_launch_template_versions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessfullyDeletedLaunchTemplateVersionSet" xml)
             DeleteLaunchTemplateVersionsResponseErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnsuccessfullyDeletedLaunchTemplateVersionSet"
              , DeleteLaunchTemplateVersionsResponseErrorSet.to_query
                  v.unsuccessfully_deleted_launch_template_versions ))
       ; Some
           (Aws.Query.Pair
              ( "SuccessfullyDeletedLaunchTemplateVersionSet"
              , DeleteLaunchTemplateVersionsResponseSuccessSet.to_query
                  v.successfully_deleted_launch_template_versions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "unsuccessfullyDeletedLaunchTemplateVersionSet"
           , DeleteLaunchTemplateVersionsResponseErrorSet.to_json
               v.unsuccessfully_deleted_launch_template_versions )
       ; Some
           ( "successfullyDeletedLaunchTemplateVersionSet"
           , DeleteLaunchTemplateVersionsResponseSuccessSet.to_json
               v.successfully_deleted_launch_template_versions )
       ])

let of_json j =
  { successfully_deleted_launch_template_versions =
      DeleteLaunchTemplateVersionsResponseSuccessSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "successfullyDeletedLaunchTemplateVersionSet"))
  ; unsuccessfully_deleted_launch_template_versions =
      DeleteLaunchTemplateVersionsResponseErrorSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "unsuccessfullyDeletedLaunchTemplateVersionSet"))
  }
