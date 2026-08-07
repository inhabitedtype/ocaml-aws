open Aws.BaseTypes

type t =
  { description : String.t
  ; group_name : String.t
  ; vpc_id : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make ~description ~group_name ?vpc_id ?(tag_specifications = []) ?dry_run () =
  { description; group_name; vpc_id; tag_specifications; dry_run }

let parse xml =
  Some
    { description =
        Aws.Xml.required
          "GroupDescription"
          (Aws.Util.option_bind (Aws.Xml.member "GroupDescription" xml) String.parse)
    ; group_name =
        Aws.Xml.required
          "GroupName"
          (Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse)
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupName", String.to_query v.group_name))
       ; Some (Aws.Query.Pair ("GroupDescription", String.to_query v.description))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Some ("GroupName", String.to_json v.group_name)
       ; Some ("GroupDescription", String.to_json v.description)
       ])

let of_json j =
  { description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupDescription"))
  ; group_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupName"))
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
