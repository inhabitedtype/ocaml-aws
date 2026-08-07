type t = { checker_ip_ranges : CheckerIpRanges.t }

let make ~checker_ip_ranges () = { checker_ip_ranges }

let parse xml =
  Some
    { checker_ip_ranges =
        Aws.Xml.required
          "CheckerIpRanges"
          (Aws.Util.option_bind
             (Aws.Xml.member "CheckerIpRanges" xml)
             CheckerIpRanges.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("CheckerIpRanges.member", CheckerIpRanges.to_query v.checker_ip_ranges))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CheckerIpRanges", CheckerIpRanges.to_json v.checker_ip_ranges) ])

let of_json j =
  { checker_ip_ranges =
      CheckerIpRanges.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CheckerIpRanges"))
  }
