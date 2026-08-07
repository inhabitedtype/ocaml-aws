open Aws.BaseTypes

type t =
  { describe_d_b_log_files : DescribeDBLogFilesList.t
  ; marker : String.t option
  }

let make ?(describe_d_b_log_files = []) ?marker () = { describe_d_b_log_files; marker }

let parse xml =
  Some
    { describe_d_b_log_files =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DescribeDBLogFiles" xml)
             DescribeDBLogFilesList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DescribeDBLogFiles.member"
              , DescribeDBLogFilesList.to_query v.describe_d_b_log_files ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some
           ("DescribeDBLogFiles", DescribeDBLogFilesList.to_json v.describe_d_b_log_files)
       ])

let of_json j =
  { describe_d_b_log_files =
      DescribeDBLogFilesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DescribeDBLogFiles"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
