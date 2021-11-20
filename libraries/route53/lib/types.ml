open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module ResourceRecord =
  struct
    type t = {
      value: String.t }
    let make ~value  () = { value }
    let parse xml =
      Some
        {
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("value", (String.to_json v.value))])
    let of_json j =
      { value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module Dimension =
  struct
    type t = {
      name: String.t ;
      value: String.t }
    let make ~name  ~value  () = { name; value }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module CloudWatchRegion =
  struct
    type t =
      | Us_east_1 
      | Us_east_2 
      | Us_west_1 
      | Us_west_2 
      | Ca_central_1 
      | Eu_central_1 
      | Eu_west_1 
      | Eu_west_2 
      | Eu_west_3 
      | Ap_east_1 
      | Me_south_1 
      | Ap_south_1 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_northeast_1 
      | Ap_northeast_2 
      | Ap_northeast_3 
      | Eu_north_1 
      | Sa_east_1 
      | Cn_northwest_1 
      | Cn_north_1 
      | Af_south_1 
      | Eu_south_1 
      | Us_gov_west_1 
      | Us_gov_east_1 
      | Us_iso_east_1 
      | Us_isob_east_1 
    let str_to_t =
      [("us-isob-east-1", Us_isob_east_1);
      ("us-iso-east-1", Us_iso_east_1);
      ("us-gov-east-1", Us_gov_east_1);
      ("us-gov-west-1", Us_gov_west_1);
      ("eu-south-1", Eu_south_1);
      ("af-south-1", Af_south_1);
      ("cn-north-1", Cn_north_1);
      ("cn-northwest-1", Cn_northwest_1);
      ("sa-east-1", Sa_east_1);
      ("eu-north-1", Eu_north_1);
      ("ap-northeast-3", Ap_northeast_3);
      ("ap-northeast-2", Ap_northeast_2);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("ap-south-1", Ap_south_1);
      ("me-south-1", Me_south_1);
      ("ap-east-1", Ap_east_1);
      ("eu-west-3", Eu_west_3);
      ("eu-west-2", Eu_west_2);
      ("eu-west-1", Eu_west_1);
      ("eu-central-1", Eu_central_1);
      ("ca-central-1", Ca_central_1);
      ("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("us-east-2", Us_east_2);
      ("us-east-1", Us_east_1)]
    let t_to_str =
      [(Us_isob_east_1, "us-isob-east-1");
      (Us_iso_east_1, "us-iso-east-1");
      (Us_gov_east_1, "us-gov-east-1");
      (Us_gov_west_1, "us-gov-west-1");
      (Eu_south_1, "eu-south-1");
      (Af_south_1, "af-south-1");
      (Cn_north_1, "cn-north-1");
      (Cn_northwest_1, "cn-northwest-1");
      (Sa_east_1, "sa-east-1");
      (Eu_north_1, "eu-north-1");
      (Ap_northeast_3, "ap-northeast-3");
      (Ap_northeast_2, "ap-northeast-2");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Ap_south_1, "ap-south-1");
      (Me_south_1, "me-south-1");
      (Ap_east_1, "ap-east-1");
      (Eu_west_3, "eu-west-3");
      (Eu_west_2, "eu-west-2");
      (Eu_west_1, "eu-west-1");
      (Eu_central_1, "eu-central-1");
      (Ca_central_1, "ca-central-1");
      (Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Us_east_2, "us-east-2");
      (Us_east_1, "us-east-1")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module HealthCheckRegion =
  struct
    type t =
      | Us_east_1 
      | Us_west_1 
      | Us_west_2 
      | Eu_west_1 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_northeast_1 
      | Sa_east_1 
    let str_to_t =
      [("sa-east-1", Sa_east_1);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("eu-west-1", Eu_west_1);
      ("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("us-east-1", Us_east_1)]
    let t_to_str =
      [(Sa_east_1, "sa-east-1");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Eu_west_1, "eu-west-1");
      (Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Us_east_1, "us-east-1")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AliasTarget =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      d_n_s_name: String.t ;
      evaluate_target_health: Boolean.t }
    let make ~hosted_zone_id  ~d_n_s_name  ~evaluate_target_health  () =
      { hosted_zone_id; d_n_s_name; evaluate_target_health }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          d_n_s_name =
            (Xml.required "DNSName"
               (Util.option_bind (Xml.member "DNSName" xml) String.parse));
          evaluate_target_health =
            (Xml.required "EvaluateTargetHealth"
               (Util.option_bind (Xml.member "EvaluateTargetHealth" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EvaluateTargetHealth",
                   (Boolean.to_query v.evaluate_target_health)));
           Some (Query.Pair ("DNSName", (String.to_query v.d_n_s_name)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("evaluate_target_health",
                (Boolean.to_json v.evaluate_target_health));
           Some ("d_n_s_name", (String.to_json v.d_n_s_name));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        d_n_s_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "d_n_s_name")));
        evaluate_target_health =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "evaluate_target_health")))
      }
  end
module GeoLocation =
  struct
    type t =
      {
      continent_code: String.t option ;
      country_code: String.t option ;
      subdivision_code: String.t option }
    let make ?continent_code  ?country_code  ?subdivision_code  () =
      { continent_code; country_code; subdivision_code }
    let parse xml =
      Some
        {
          continent_code =
            (Util.option_bind (Xml.member "ContinentCode" xml) String.parse);
          country_code =
            (Util.option_bind (Xml.member "CountryCode" xml) String.parse);
          subdivision_code =
            (Util.option_bind (Xml.member "SubdivisionCode" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subdivision_code
              (fun f -> Query.Pair ("SubdivisionCode", (String.to_query f)));
           Util.option_map v.country_code
             (fun f -> Query.Pair ("CountryCode", (String.to_query f)));
           Util.option_map v.continent_code
             (fun f -> Query.Pair ("ContinentCode", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subdivision_code
              (fun f -> ("subdivision_code", (String.to_json f)));
           Util.option_map v.country_code
             (fun f -> ("country_code", (String.to_json f)));
           Util.option_map v.continent_code
             (fun f -> ("continent_code", (String.to_json f)))])
    let of_json j =
      {
        continent_code =
          (Util.option_map (Json.lookup j "continent_code") String.of_json);
        country_code =
          (Util.option_map (Json.lookup j "country_code") String.of_json);
        subdivision_code =
          (Util.option_map (Json.lookup j "subdivision_code") String.of_json)
      }
  end
module RRType =
  struct
    type t =
      | SOA 
      | A 
      | TXT 
      | NS 
      | CNAME 
      | MX 
      | NAPTR 
      | PTR 
      | SRV 
      | SPF 
      | AAAA 
      | CAA 
    let str_to_t =
      [("CAA", CAA);
      ("AAAA", AAAA);
      ("SPF", SPF);
      ("SRV", SRV);
      ("PTR", PTR);
      ("NAPTR", NAPTR);
      ("MX", MX);
      ("CNAME", CNAME);
      ("NS", NS);
      ("TXT", TXT);
      ("A", A);
      ("SOA", SOA)]
    let t_to_str =
      [(CAA, "CAA");
      (AAAA, "AAAA");
      (SPF, "SPF");
      (SRV, "SRV");
      (PTR, "PTR");
      (NAPTR, "NAPTR");
      (MX, "MX");
      (CNAME, "CNAME");
      (NS, "NS");
      (TXT, "TXT");
      (A, "A");
      (SOA, "SOA")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceRecordSetFailover =
  struct
    type t =
      | PRIMARY 
      | SECONDARY 
    let str_to_t = [("SECONDARY", SECONDARY); ("PRIMARY", PRIMARY)]
    let t_to_str = [(SECONDARY, "SECONDARY"); (PRIMARY, "PRIMARY")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceRecordSetRegion =
  struct
    type t =
      | Us_east_1 
      | Us_east_2 
      | Us_west_1 
      | Us_west_2 
      | Ca_central_1 
      | Eu_west_1 
      | Eu_west_2 
      | Eu_west_3 
      | Eu_central_1 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_northeast_1 
      | Ap_northeast_2 
      | Ap_northeast_3 
      | Eu_north_1 
      | Sa_east_1 
      | Cn_north_1 
      | Cn_northwest_1 
      | Ap_east_1 
      | Me_south_1 
      | Ap_south_1 
      | Af_south_1 
      | Eu_south_1 
    let str_to_t =
      [("eu-south-1", Eu_south_1);
      ("af-south-1", Af_south_1);
      ("ap-south-1", Ap_south_1);
      ("me-south-1", Me_south_1);
      ("ap-east-1", Ap_east_1);
      ("cn-northwest-1", Cn_northwest_1);
      ("cn-north-1", Cn_north_1);
      ("sa-east-1", Sa_east_1);
      ("eu-north-1", Eu_north_1);
      ("ap-northeast-3", Ap_northeast_3);
      ("ap-northeast-2", Ap_northeast_2);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("eu-central-1", Eu_central_1);
      ("eu-west-3", Eu_west_3);
      ("eu-west-2", Eu_west_2);
      ("eu-west-1", Eu_west_1);
      ("ca-central-1", Ca_central_1);
      ("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("us-east-2", Us_east_2);
      ("us-east-1", Us_east_1)]
    let t_to_str =
      [(Eu_south_1, "eu-south-1");
      (Af_south_1, "af-south-1");
      (Ap_south_1, "ap-south-1");
      (Me_south_1, "me-south-1");
      (Ap_east_1, "ap-east-1");
      (Cn_northwest_1, "cn-northwest-1");
      (Cn_north_1, "cn-north-1");
      (Sa_east_1, "sa-east-1");
      (Eu_north_1, "eu-north-1");
      (Ap_northeast_3, "ap-northeast-3");
      (Ap_northeast_2, "ap-northeast-2");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Eu_central_1, "eu-central-1");
      (Eu_west_3, "eu-west-3");
      (Eu_west_2, "eu-west-2");
      (Eu_west_1, "eu-west-1");
      (Ca_central_1, "ca-central-1");
      (Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Us_east_2, "us-east-2");
      (Us_east_1, "us-east-1")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceRecords =
  struct
    type t = ResourceRecord.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceRecord.parse (Xml.members "ResourceRecord" xml))
    let to_query v = Query.to_query_list ResourceRecord.to_query v
    let to_json v = `List (List.map ResourceRecord.to_json v)
    let of_json j = Json.to_list ResourceRecord.of_json j
  end
module ComparisonOperator =
  struct
    type t =
      | GreaterThanOrEqualToThreshold 
      | GreaterThanThreshold 
      | LessThanThreshold 
      | LessThanOrEqualToThreshold 
    let str_to_t =
      [("LessThanOrEqualToThreshold", LessThanOrEqualToThreshold);
      ("LessThanThreshold", LessThanThreshold);
      ("GreaterThanThreshold", GreaterThanThreshold);
      ("GreaterThanOrEqualToThreshold", GreaterThanOrEqualToThreshold)]
    let t_to_str =
      [(LessThanOrEqualToThreshold, "LessThanOrEqualToThreshold");
      (LessThanThreshold, "LessThanThreshold");
      (GreaterThanThreshold, "GreaterThanThreshold");
      (GreaterThanOrEqualToThreshold, "GreaterThanOrEqualToThreshold")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module DimensionList =
  struct
    type t = Dimension.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Dimension.parse (Xml.members "Dimension" xml))
    let to_query v = Query.to_query_list Dimension.to_query v
    let to_json v = `List (List.map Dimension.to_json v)
    let of_json j = Json.to_list Dimension.of_json j
  end
module Statistic =
  struct
    type t =
      | Average 
      | Sum 
      | SampleCount 
      | Maximum 
      | Minimum 
    let str_to_t =
      [("Minimum", Minimum);
      ("Maximum", Maximum);
      ("SampleCount", SampleCount);
      ("Sum", Sum);
      ("Average", Average)]
    let t_to_str =
      [(Minimum, "Minimum");
      (Maximum, "Maximum");
      (SampleCount, "SampleCount");
      (Sum, "Sum");
      (Average, "Average")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module AlarmIdentifier =
  struct
    type t = {
      region: CloudWatchRegion.t ;
      name: String.t }
    let make ~region  ~name  () = { region; name }
    let parse xml =
      Some
        {
          region =
            (Xml.required "Region"
               (Util.option_bind (Xml.member "Region" xml)
                  CloudWatchRegion.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Region", (CloudWatchRegion.to_query v.region)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("name", (String.to_json v.name));
           Some ("region", (CloudWatchRegion.to_json v.region))])
    let of_json j =
      {
        region =
          (CloudWatchRegion.of_json
             (Util.of_option_exn (Json.lookup j "region")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")))
      }
  end
module ChildHealthCheckList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "ChildHealthCheck" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module HealthCheckRegionList =
  struct
    type t = HealthCheckRegion.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map HealthCheckRegion.parse (Xml.members "Region" xml))
    let to_query v = Query.to_query_list HealthCheckRegion.to_query v
    let to_json v = `List (List.map HealthCheckRegion.to_json v)
    let of_json j = Json.to_list HealthCheckRegion.of_json j
  end
module HealthCheckType =
  struct
    type t =
      | HTTP 
      | HTTPS 
      | HTTP_STR_MATCH 
      | HTTPS_STR_MATCH 
      | TCP 
      | CALCULATED 
      | CLOUDWATCH_METRIC 
    let str_to_t =
      [("CLOUDWATCH_METRIC", CLOUDWATCH_METRIC);
      ("CALCULATED", CALCULATED);
      ("TCP", TCP);
      ("HTTPS_STR_MATCH", HTTPS_STR_MATCH);
      ("HTTP_STR_MATCH", HTTP_STR_MATCH);
      ("HTTPS", HTTPS);
      ("HTTP", HTTP)]
    let t_to_str =
      [(CLOUDWATCH_METRIC, "CLOUDWATCH_METRIC");
      (CALCULATED, "CALCULATED");
      (TCP, "TCP");
      (HTTPS_STR_MATCH, "HTTPS_STR_MATCH");
      (HTTP_STR_MATCH, "HTTP_STR_MATCH");
      (HTTPS, "HTTPS");
      (HTTP, "HTTP")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module InsufficientDataHealthStatus =
  struct
    type t =
      | Healthy 
      | Unhealthy 
      | LastKnownStatus 
    let str_to_t =
      [("LastKnownStatus", LastKnownStatus);
      ("Unhealthy", Unhealthy);
      ("Healthy", Healthy)]
    let t_to_str =
      [(LastKnownStatus, "LastKnownStatus");
      (Unhealthy, "Unhealthy");
      (Healthy, "Healthy")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Tag =
  struct
    type t = {
      key: String.t option ;
      value: String.t option }
    let make ?key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module ChangeAction =
  struct
    type t =
      | CREATE 
      | DELETE 
      | UPSERT 
    let str_to_t =
      [("UPSERT", UPSERT); ("DELETE", DELETE); ("CREATE", CREATE)]
    let t_to_str =
      [(UPSERT, "UPSERT"); (DELETE, "DELETE"); (CREATE, "CREATE")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ResourceRecordSet =
  struct
    type t =
      {
      name: String.t ;
      type_: RRType.t ;
      set_identifier: String.t option ;
      weight: Long.t option ;
      region: ResourceRecordSetRegion.t option ;
      geo_location: GeoLocation.t option ;
      failover: ResourceRecordSetFailover.t option ;
      multi_value_answer: Boolean.t option ;
      t_t_l: Long.t option ;
      resource_records: ResourceRecords.t ;
      alias_target: AliasTarget.t option ;
      health_check_id: String.t option ;
      traffic_policy_instance_id: String.t option }
    let make ~name  ~type_  ?set_identifier  ?weight  ?region  ?geo_location 
      ?failover  ?multi_value_answer  ?t_t_l  ?(resource_records= []) 
      ?alias_target  ?health_check_id  ?traffic_policy_instance_id  () =
      {
        name;
        type_;
        set_identifier;
        weight;
        region;
        geo_location;
        failover;
        multi_value_answer;
        t_t_l;
        resource_records;
        alias_target;
        health_check_id;
        traffic_policy_instance_id
      }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml) RRType.parse));
          set_identifier =
            (Util.option_bind (Xml.member "SetIdentifier" xml) String.parse);
          weight = (Util.option_bind (Xml.member "Weight" xml) Long.parse);
          region =
            (Util.option_bind (Xml.member "Region" xml)
               ResourceRecordSetRegion.parse);
          geo_location =
            (Util.option_bind (Xml.member "GeoLocation" xml)
               GeoLocation.parse);
          failover =
            (Util.option_bind (Xml.member "Failover" xml)
               ResourceRecordSetFailover.parse);
          multi_value_answer =
            (Util.option_bind (Xml.member "MultiValueAnswer" xml)
               Boolean.parse);
          t_t_l = (Util.option_bind (Xml.member "TTL" xml) Long.parse);
          resource_records =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceRecords" xml)
                  ResourceRecords.parse));
          alias_target =
            (Util.option_bind (Xml.member "AliasTarget" xml)
               AliasTarget.parse);
          health_check_id =
            (Util.option_bind (Xml.member "HealthCheckId" xml) String.parse);
          traffic_policy_instance_id =
            (Util.option_bind (Xml.member "TrafficPolicyInstanceId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.traffic_policy_instance_id
              (fun f ->
                 Query.Pair ("TrafficPolicyInstanceId", (String.to_query f)));
           Util.option_map v.health_check_id
             (fun f -> Query.Pair ("HealthCheckId", (String.to_query f)));
           Util.option_map v.alias_target
             (fun f -> Query.Pair ("AliasTarget", (AliasTarget.to_query f)));
           Some
             (Query.Pair
                ("ResourceRecords.member",
                  (ResourceRecords.to_query v.resource_records)));
           Util.option_map v.t_t_l
             (fun f -> Query.Pair ("TTL", (Long.to_query f)));
           Util.option_map v.multi_value_answer
             (fun f -> Query.Pair ("MultiValueAnswer", (Boolean.to_query f)));
           Util.option_map v.failover
             (fun f ->
                Query.Pair
                  ("Failover", (ResourceRecordSetFailover.to_query f)));
           Util.option_map v.geo_location
             (fun f -> Query.Pair ("GeoLocation", (GeoLocation.to_query f)));
           Util.option_map v.region
             (fun f ->
                Query.Pair ("Region", (ResourceRecordSetRegion.to_query f)));
           Util.option_map v.weight
             (fun f -> Query.Pair ("Weight", (Long.to_query f)));
           Util.option_map v.set_identifier
             (fun f -> Query.Pair ("SetIdentifier", (String.to_query f)));
           Some (Query.Pair ("Type", (RRType.to_query v.type_)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.traffic_policy_instance_id
              (fun f -> ("traffic_policy_instance_id", (String.to_json f)));
           Util.option_map v.health_check_id
             (fun f -> ("health_check_id", (String.to_json f)));
           Util.option_map v.alias_target
             (fun f -> ("alias_target", (AliasTarget.to_json f)));
           Some
             ("resource_records",
               (ResourceRecords.to_json v.resource_records));
           Util.option_map v.t_t_l (fun f -> ("t_t_l", (Long.to_json f)));
           Util.option_map v.multi_value_answer
             (fun f -> ("multi_value_answer", (Boolean.to_json f)));
           Util.option_map v.failover
             (fun f -> ("failover", (ResourceRecordSetFailover.to_json f)));
           Util.option_map v.geo_location
             (fun f -> ("geo_location", (GeoLocation.to_json f)));
           Util.option_map v.region
             (fun f -> ("region", (ResourceRecordSetRegion.to_json f)));
           Util.option_map v.weight (fun f -> ("weight", (Long.to_json f)));
           Util.option_map v.set_identifier
             (fun f -> ("set_identifier", (String.to_json f)));
           Some ("type_", (RRType.to_json v.type_));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        type_ = (RRType.of_json (Util.of_option_exn (Json.lookup j "type_")));
        set_identifier =
          (Util.option_map (Json.lookup j "set_identifier") String.of_json);
        weight = (Util.option_map (Json.lookup j "weight") Long.of_json);
        region =
          (Util.option_map (Json.lookup j "region")
             ResourceRecordSetRegion.of_json);
        geo_location =
          (Util.option_map (Json.lookup j "geo_location") GeoLocation.of_json);
        failover =
          (Util.option_map (Json.lookup j "failover")
             ResourceRecordSetFailover.of_json);
        multi_value_answer =
          (Util.option_map (Json.lookup j "multi_value_answer")
             Boolean.of_json);
        t_t_l = (Util.option_map (Json.lookup j "t_t_l") Long.of_json);
        resource_records =
          (ResourceRecords.of_json
             (Util.of_option_exn (Json.lookup j "resource_records")));
        alias_target =
          (Util.option_map (Json.lookup j "alias_target") AliasTarget.of_json);
        health_check_id =
          (Util.option_map (Json.lookup j "health_check_id") String.of_json);
        traffic_policy_instance_id =
          (Util.option_map (Json.lookup j "traffic_policy_instance_id")
             String.of_json)
      }
  end
module HostedZoneConfig =
  struct
    type t = {
      comment: String.t option ;
      private_zone: Boolean.t option }
    let make ?comment  ?private_zone  () = { comment; private_zone }
    let parse xml =
      Some
        {
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse);
          private_zone =
            (Util.option_bind (Xml.member "PrivateZone" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.private_zone
              (fun f -> Query.Pair ("PrivateZone", (Boolean.to_query f)));
           Util.option_map v.comment
             (fun f -> Query.Pair ("Comment", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.private_zone
              (fun f -> ("private_zone", (Boolean.to_json f)));
           Util.option_map v.comment
             (fun f -> ("comment", (String.to_json f)))])
    let of_json j =
      {
        comment = (Util.option_map (Json.lookup j "comment") String.of_json);
        private_zone =
          (Util.option_map (Json.lookup j "private_zone") Boolean.of_json)
      }
  end
module LinkedService =
  struct
    type t =
      {
      service_principal: String.t option ;
      description: String.t option }
    let make ?service_principal  ?description  () =
      { service_principal; description }
    let parse xml =
      Some
        {
          service_principal =
            (Util.option_bind (Xml.member "ServicePrincipal" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.service_principal
             (fun f -> Query.Pair ("ServicePrincipal", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.service_principal
             (fun f -> ("service_principal", (String.to_json f)))])
    let of_json j =
      {
        service_principal =
          (Util.option_map (Json.lookup j "service_principal") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module StatusReport =
  struct
    type t = {
      status: String.t option ;
      checked_time: DateTime.t option }
    let make ?status  ?checked_time  () = { status; checked_time }
    let parse xml =
      Some
        {
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          checked_time =
            (Util.option_bind (Xml.member "CheckedTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.checked_time
              (fun f -> Query.Pair ("CheckedTime", (DateTime.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.checked_time
              (fun f -> ("checked_time", (DateTime.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)))])
    let of_json j =
      {
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        checked_time =
          (Util.option_map (Json.lookup j "checked_time") DateTime.of_json)
      }
  end
module HostedZoneOwner =
  struct
    type t =
      {
      owning_account: String.t option ;
      owning_service: String.t option }
    let make ?owning_account  ?owning_service  () =
      { owning_account; owning_service }
    let parse xml =
      Some
        {
          owning_account =
            (Util.option_bind (Xml.member "OwningAccount" xml) String.parse);
          owning_service =
            (Util.option_bind (Xml.member "OwningService" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.owning_service
              (fun f -> Query.Pair ("OwningService", (String.to_query f)));
           Util.option_map v.owning_account
             (fun f -> Query.Pair ("OwningAccount", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.owning_service
              (fun f -> ("owning_service", (String.to_json f)));
           Util.option_map v.owning_account
             (fun f -> ("owning_account", (String.to_json f)))])
    let of_json j =
      {
        owning_account =
          (Util.option_map (Json.lookup j "owning_account") String.of_json);
        owning_service =
          (Util.option_map (Json.lookup j "owning_service") String.of_json)
      }
  end
module VPCRegion =
  struct
    type t =
      | Us_east_1 
      | Us_east_2 
      | Us_west_1 
      | Us_west_2 
      | Eu_west_1 
      | Eu_west_2 
      | Eu_west_3 
      | Eu_central_1 
      | Ap_east_1 
      | Me_south_1 
      | Us_gov_west_1 
      | Us_gov_east_1 
      | Us_iso_east_1 
      | Us_isob_east_1 
      | Ap_southeast_1 
      | Ap_southeast_2 
      | Ap_south_1 
      | Ap_northeast_1 
      | Ap_northeast_2 
      | Ap_northeast_3 
      | Eu_north_1 
      | Sa_east_1 
      | Ca_central_1 
      | Cn_north_1 
      | Af_south_1 
      | Eu_south_1 
    let str_to_t =
      [("eu-south-1", Eu_south_1);
      ("af-south-1", Af_south_1);
      ("cn-north-1", Cn_north_1);
      ("ca-central-1", Ca_central_1);
      ("sa-east-1", Sa_east_1);
      ("eu-north-1", Eu_north_1);
      ("ap-northeast-3", Ap_northeast_3);
      ("ap-northeast-2", Ap_northeast_2);
      ("ap-northeast-1", Ap_northeast_1);
      ("ap-south-1", Ap_south_1);
      ("ap-southeast-2", Ap_southeast_2);
      ("ap-southeast-1", Ap_southeast_1);
      ("us-isob-east-1", Us_isob_east_1);
      ("us-iso-east-1", Us_iso_east_1);
      ("us-gov-east-1", Us_gov_east_1);
      ("us-gov-west-1", Us_gov_west_1);
      ("me-south-1", Me_south_1);
      ("ap-east-1", Ap_east_1);
      ("eu-central-1", Eu_central_1);
      ("eu-west-3", Eu_west_3);
      ("eu-west-2", Eu_west_2);
      ("eu-west-1", Eu_west_1);
      ("us-west-2", Us_west_2);
      ("us-west-1", Us_west_1);
      ("us-east-2", Us_east_2);
      ("us-east-1", Us_east_1)]
    let t_to_str =
      [(Eu_south_1, "eu-south-1");
      (Af_south_1, "af-south-1");
      (Cn_north_1, "cn-north-1");
      (Ca_central_1, "ca-central-1");
      (Sa_east_1, "sa-east-1");
      (Eu_north_1, "eu-north-1");
      (Ap_northeast_3, "ap-northeast-3");
      (Ap_northeast_2, "ap-northeast-2");
      (Ap_northeast_1, "ap-northeast-1");
      (Ap_south_1, "ap-south-1");
      (Ap_southeast_2, "ap-southeast-2");
      (Ap_southeast_1, "ap-southeast-1");
      (Us_isob_east_1, "us-isob-east-1");
      (Us_iso_east_1, "us-iso-east-1");
      (Us_gov_east_1, "us-gov-east-1");
      (Us_gov_west_1, "us-gov-west-1");
      (Me_south_1, "me-south-1");
      (Ap_east_1, "ap-east-1");
      (Eu_central_1, "eu-central-1");
      (Eu_west_3, "eu-west-3");
      (Eu_west_2, "eu-west-2");
      (Eu_west_1, "eu-west-1");
      (Us_west_2, "us-west-2");
      (Us_west_1, "us-west-1");
      (Us_east_2, "us-east-2");
      (Us_east_1, "us-east-1")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CloudWatchAlarmConfiguration =
  struct
    type t =
      {
      evaluation_periods: Integer.t ;
      threshold: Double.t ;
      comparison_operator: ComparisonOperator.t ;
      period: Integer.t ;
      metric_name: String.t ;
      namespace: String.t ;
      statistic: Statistic.t ;
      dimensions: DimensionList.t }
    let make ~evaluation_periods  ~threshold  ~comparison_operator  ~period 
      ~metric_name  ~namespace  ~statistic  ?(dimensions= [])  () =
      {
        evaluation_periods;
        threshold;
        comparison_operator;
        period;
        metric_name;
        namespace;
        statistic;
        dimensions
      }
    let parse xml =
      Some
        {
          evaluation_periods =
            (Xml.required "EvaluationPeriods"
               (Util.option_bind (Xml.member "EvaluationPeriods" xml)
                  Integer.parse));
          threshold =
            (Xml.required "Threshold"
               (Util.option_bind (Xml.member "Threshold" xml) Double.parse));
          comparison_operator =
            (Xml.required "ComparisonOperator"
               (Util.option_bind (Xml.member "ComparisonOperator" xml)
                  ComparisonOperator.parse));
          period =
            (Xml.required "Period"
               (Util.option_bind (Xml.member "Period" xml) Integer.parse));
          metric_name =
            (Xml.required "MetricName"
               (Util.option_bind (Xml.member "MetricName" xml) String.parse));
          namespace =
            (Xml.required "Namespace"
               (Util.option_bind (Xml.member "Namespace" xml) String.parse));
          statistic =
            (Xml.required "Statistic"
               (Util.option_bind (Xml.member "Statistic" xml) Statistic.parse));
          dimensions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Dimensions" xml)
                  DimensionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Dimensions.member", (DimensionList.to_query v.dimensions)));
           Some (Query.Pair ("Statistic", (Statistic.to_query v.statistic)));
           Some (Query.Pair ("Namespace", (String.to_query v.namespace)));
           Some (Query.Pair ("MetricName", (String.to_query v.metric_name)));
           Some (Query.Pair ("Period", (Integer.to_query v.period)));
           Some
             (Query.Pair
                ("ComparisonOperator",
                  (ComparisonOperator.to_query v.comparison_operator)));
           Some (Query.Pair ("Threshold", (Double.to_query v.threshold)));
           Some
             (Query.Pair
                ("EvaluationPeriods",
                  (Integer.to_query v.evaluation_periods)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("dimensions", (DimensionList.to_json v.dimensions));
           Some ("statistic", (Statistic.to_json v.statistic));
           Some ("namespace", (String.to_json v.namespace));
           Some ("metric_name", (String.to_json v.metric_name));
           Some ("period", (Integer.to_json v.period));
           Some
             ("comparison_operator",
               (ComparisonOperator.to_json v.comparison_operator));
           Some ("threshold", (Double.to_json v.threshold));
           Some
             ("evaluation_periods", (Integer.to_json v.evaluation_periods))])
    let of_json j =
      {
        evaluation_periods =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "evaluation_periods")));
        threshold =
          (Double.of_json (Util.of_option_exn (Json.lookup j "threshold")));
        comparison_operator =
          (ComparisonOperator.of_json
             (Util.of_option_exn (Json.lookup j "comparison_operator")));
        period =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "period")));
        metric_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "metric_name")));
        namespace =
          (String.of_json (Util.of_option_exn (Json.lookup j "namespace")));
        statistic =
          (Statistic.of_json (Util.of_option_exn (Json.lookup j "statistic")));
        dimensions =
          (DimensionList.of_json
             (Util.of_option_exn (Json.lookup j "dimensions")))
      }
  end
module HealthCheckConfig =
  struct
    type t =
      {
      i_p_address: String.t option ;
      port: Integer.t option ;
      type_: HealthCheckType.t ;
      resource_path: String.t option ;
      fully_qualified_domain_name: String.t option ;
      search_string: String.t option ;
      request_interval: Integer.t option ;
      failure_threshold: Integer.t option ;
      measure_latency: Boolean.t option ;
      inverted: Boolean.t option ;
      disabled: Boolean.t option ;
      health_threshold: Integer.t option ;
      child_health_checks: ChildHealthCheckList.t ;
      enable_s_n_i: Boolean.t option ;
      regions: HealthCheckRegionList.t ;
      alarm_identifier: AlarmIdentifier.t option ;
      insufficient_data_health_status: InsufficientDataHealthStatus.t option }
    let make ?i_p_address  ?port  ~type_  ?resource_path 
      ?fully_qualified_domain_name  ?search_string  ?request_interval 
      ?failure_threshold  ?measure_latency  ?inverted  ?disabled 
      ?health_threshold  ?(child_health_checks= [])  ?enable_s_n_i 
      ?(regions= [])  ?alarm_identifier  ?insufficient_data_health_status  ()
      =
      {
        i_p_address;
        port;
        type_;
        resource_path;
        fully_qualified_domain_name;
        search_string;
        request_interval;
        failure_threshold;
        measure_latency;
        inverted;
        disabled;
        health_threshold;
        child_health_checks;
        enable_s_n_i;
        regions;
        alarm_identifier;
        insufficient_data_health_status
      }
    let parse xml =
      Some
        {
          i_p_address =
            (Util.option_bind (Xml.member "IPAddress" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  HealthCheckType.parse));
          resource_path =
            (Util.option_bind (Xml.member "ResourcePath" xml) String.parse);
          fully_qualified_domain_name =
            (Util.option_bind (Xml.member "FullyQualifiedDomainName" xml)
               String.parse);
          search_string =
            (Util.option_bind (Xml.member "SearchString" xml) String.parse);
          request_interval =
            (Util.option_bind (Xml.member "RequestInterval" xml)
               Integer.parse);
          failure_threshold =
            (Util.option_bind (Xml.member "FailureThreshold" xml)
               Integer.parse);
          measure_latency =
            (Util.option_bind (Xml.member "MeasureLatency" xml) Boolean.parse);
          inverted =
            (Util.option_bind (Xml.member "Inverted" xml) Boolean.parse);
          disabled =
            (Util.option_bind (Xml.member "Disabled" xml) Boolean.parse);
          health_threshold =
            (Util.option_bind (Xml.member "HealthThreshold" xml)
               Integer.parse);
          child_health_checks =
            (Util.of_option []
               (Util.option_bind (Xml.member "ChildHealthChecks" xml)
                  ChildHealthCheckList.parse));
          enable_s_n_i =
            (Util.option_bind (Xml.member "EnableSNI" xml) Boolean.parse);
          regions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Regions" xml)
                  HealthCheckRegionList.parse));
          alarm_identifier =
            (Util.option_bind (Xml.member "AlarmIdentifier" xml)
               AlarmIdentifier.parse);
          insufficient_data_health_status =
            (Util.option_bind (Xml.member "InsufficientDataHealthStatus" xml)
               InsufficientDataHealthStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.insufficient_data_health_status
              (fun f ->
                 Query.Pair
                   ("InsufficientDataHealthStatus",
                     (InsufficientDataHealthStatus.to_query f)));
           Util.option_map v.alarm_identifier
             (fun f ->
                Query.Pair ("AlarmIdentifier", (AlarmIdentifier.to_query f)));
           Some
             (Query.Pair
                ("Regions.member",
                  (HealthCheckRegionList.to_query v.regions)));
           Util.option_map v.enable_s_n_i
             (fun f -> Query.Pair ("EnableSNI", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("ChildHealthChecks.member",
                  (ChildHealthCheckList.to_query v.child_health_checks)));
           Util.option_map v.health_threshold
             (fun f -> Query.Pair ("HealthThreshold", (Integer.to_query f)));
           Util.option_map v.disabled
             (fun f -> Query.Pair ("Disabled", (Boolean.to_query f)));
           Util.option_map v.inverted
             (fun f -> Query.Pair ("Inverted", (Boolean.to_query f)));
           Util.option_map v.measure_latency
             (fun f -> Query.Pair ("MeasureLatency", (Boolean.to_query f)));
           Util.option_map v.failure_threshold
             (fun f -> Query.Pair ("FailureThreshold", (Integer.to_query f)));
           Util.option_map v.request_interval
             (fun f -> Query.Pair ("RequestInterval", (Integer.to_query f)));
           Util.option_map v.search_string
             (fun f -> Query.Pair ("SearchString", (String.to_query f)));
           Util.option_map v.fully_qualified_domain_name
             (fun f ->
                Query.Pair ("FullyQualifiedDomainName", (String.to_query f)));
           Util.option_map v.resource_path
             (fun f -> Query.Pair ("ResourcePath", (String.to_query f)));
           Some (Query.Pair ("Type", (HealthCheckType.to_query v.type_)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.i_p_address
             (fun f -> Query.Pair ("IPAddress", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.insufficient_data_health_status
              (fun f ->
                 ("insufficient_data_health_status",
                   (InsufficientDataHealthStatus.to_json f)));
           Util.option_map v.alarm_identifier
             (fun f -> ("alarm_identifier", (AlarmIdentifier.to_json f)));
           Some ("regions", (HealthCheckRegionList.to_json v.regions));
           Util.option_map v.enable_s_n_i
             (fun f -> ("enable_s_n_i", (Boolean.to_json f)));
           Some
             ("child_health_checks",
               (ChildHealthCheckList.to_json v.child_health_checks));
           Util.option_map v.health_threshold
             (fun f -> ("health_threshold", (Integer.to_json f)));
           Util.option_map v.disabled
             (fun f -> ("disabled", (Boolean.to_json f)));
           Util.option_map v.inverted
             (fun f -> ("inverted", (Boolean.to_json f)));
           Util.option_map v.measure_latency
             (fun f -> ("measure_latency", (Boolean.to_json f)));
           Util.option_map v.failure_threshold
             (fun f -> ("failure_threshold", (Integer.to_json f)));
           Util.option_map v.request_interval
             (fun f -> ("request_interval", (Integer.to_json f)));
           Util.option_map v.search_string
             (fun f -> ("search_string", (String.to_json f)));
           Util.option_map v.fully_qualified_domain_name
             (fun f -> ("fully_qualified_domain_name", (String.to_json f)));
           Util.option_map v.resource_path
             (fun f -> ("resource_path", (String.to_json f)));
           Some ("type_", (HealthCheckType.to_json v.type_));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.i_p_address
             (fun f -> ("i_p_address", (String.to_json f)))])
    let of_json j =
      {
        i_p_address =
          (Util.option_map (Json.lookup j "i_p_address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        type_ =
          (HealthCheckType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        resource_path =
          (Util.option_map (Json.lookup j "resource_path") String.of_json);
        fully_qualified_domain_name =
          (Util.option_map (Json.lookup j "fully_qualified_domain_name")
             String.of_json);
        search_string =
          (Util.option_map (Json.lookup j "search_string") String.of_json);
        request_interval =
          (Util.option_map (Json.lookup j "request_interval") Integer.of_json);
        failure_threshold =
          (Util.option_map (Json.lookup j "failure_threshold")
             Integer.of_json);
        measure_latency =
          (Util.option_map (Json.lookup j "measure_latency") Boolean.of_json);
        inverted =
          (Util.option_map (Json.lookup j "inverted") Boolean.of_json);
        disabled =
          (Util.option_map (Json.lookup j "disabled") Boolean.of_json);
        health_threshold =
          (Util.option_map (Json.lookup j "health_threshold") Integer.of_json);
        child_health_checks =
          (ChildHealthCheckList.of_json
             (Util.of_option_exn (Json.lookup j "child_health_checks")));
        enable_s_n_i =
          (Util.option_map (Json.lookup j "enable_s_n_i") Boolean.of_json);
        regions =
          (HealthCheckRegionList.of_json
             (Util.of_option_exn (Json.lookup j "regions")));
        alarm_identifier =
          (Util.option_map (Json.lookup j "alarm_identifier")
             AlarmIdentifier.of_json);
        insufficient_data_health_status =
          (Util.option_map (Json.lookup j "insufficient_data_health_status")
             InsufficientDataHealthStatus.of_json)
      }
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "Tag" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module TagResourceType =
  struct
    type t =
      | Healthcheck 
      | Hostedzone 
    let str_to_t = [("hostedzone", Hostedzone); ("healthcheck", Healthcheck)]
    let t_to_str = [(Hostedzone, "hostedzone"); (Healthcheck, "healthcheck")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module DelegationSetNameServers =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "NameServer" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Change =
  struct
    type t =
      {
      action: ChangeAction.t ;
      resource_record_set: ResourceRecordSet.t }
    let make ~action  ~resource_record_set  () =
      { action; resource_record_set }
    let parse xml =
      Some
        {
          action =
            (Xml.required "Action"
               (Util.option_bind (Xml.member "Action" xml) ChangeAction.parse));
          resource_record_set =
            (Xml.required "ResourceRecordSet"
               (Util.option_bind (Xml.member "ResourceRecordSet" xml)
                  ResourceRecordSet.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceRecordSet",
                   (ResourceRecordSet.to_query v.resource_record_set)));
           Some (Query.Pair ("Action", (ChangeAction.to_query v.action)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_record_set",
                (ResourceRecordSet.to_json v.resource_record_set));
           Some ("action", (ChangeAction.to_json v.action))])
    let of_json j =
      {
        action =
          (ChangeAction.of_json (Util.of_option_exn (Json.lookup j "action")));
        resource_record_set =
          (ResourceRecordSet.of_json
             (Util.of_option_exn (Json.lookup j "resource_record_set")))
      }
  end
module HostedZone =
  struct
    type t =
      {
      id: String.t ;
      name: String.t ;
      caller_reference: String.t ;
      config: HostedZoneConfig.t option ;
      resource_record_set_count: Long.t option ;
      linked_service: LinkedService.t option }
    let make ~id  ~name  ~caller_reference  ?config 
      ?resource_record_set_count  ?linked_service  () =
      {
        id;
        name;
        caller_reference;
        config;
        resource_record_set_count;
        linked_service
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          config =
            (Util.option_bind (Xml.member "Config" xml)
               HostedZoneConfig.parse);
          resource_record_set_count =
            (Util.option_bind (Xml.member "ResourceRecordSetCount" xml)
               Long.parse);
          linked_service =
            (Util.option_bind (Xml.member "LinkedService" xml)
               LinkedService.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.linked_service
              (fun f ->
                 Query.Pair ("LinkedService", (LinkedService.to_query f)));
           Util.option_map v.resource_record_set_count
             (fun f ->
                Query.Pair ("ResourceRecordSetCount", (Long.to_query f)));
           Util.option_map v.config
             (fun f -> Query.Pair ("Config", (HostedZoneConfig.to_query f)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.linked_service
              (fun f -> ("linked_service", (LinkedService.to_json f)));
           Util.option_map v.resource_record_set_count
             (fun f -> ("resource_record_set_count", (Long.to_json f)));
           Util.option_map v.config
             (fun f -> ("config", (HostedZoneConfig.to_json f)));
           Some ("caller_reference", (String.to_json v.caller_reference));
           Some ("name", (String.to_json v.name));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        config =
          (Util.option_map (Json.lookup j "config") HostedZoneConfig.of_json);
        resource_record_set_count =
          (Util.option_map (Json.lookup j "resource_record_set_count")
             Long.of_json);
        linked_service =
          (Util.option_map (Json.lookup j "linked_service")
             LinkedService.of_json)
      }
  end
module ChangeStatus =
  struct
    type t =
      | PENDING 
      | INSYNC 
    let str_to_t = [("INSYNC", INSYNC); ("PENDING", PENDING)]
    let t_to_str = [(INSYNC, "INSYNC"); (PENDING, "PENDING")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module TrafficPolicy =
  struct
    type t =
      {
      id: String.t ;
      version: Integer.t ;
      name: String.t ;
      type_: RRType.t ;
      document: String.t ;
      comment: String.t option }
    let make ~id  ~version  ~name  ~type_  ~document  ?comment  () =
      { id; version; name; type_; document; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          version =
            (Xml.required "Version"
               (Util.option_bind (Xml.member "Version" xml) Integer.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml) RRType.parse));
          document =
            (Xml.required "Document"
               (Util.option_bind (Xml.member "Document" xml) String.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("Document", (String.to_query v.document)));
           Some (Query.Pair ("Type", (RRType.to_query v.type_)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Version", (Integer.to_query v.version)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("document", (String.to_json v.document));
           Some ("type_", (RRType.to_json v.type_));
           Some ("name", (String.to_json v.name));
           Some ("version", (Integer.to_json v.version));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        version =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "version")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        type_ = (RRType.of_json (Util.of_option_exn (Json.lookup j "type_")));
        document =
          (String.of_json (Util.of_option_exn (Json.lookup j "document")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module HealthCheckObservation =
  struct
    type t =
      {
      region: HealthCheckRegion.t option ;
      i_p_address: String.t option ;
      status_report: StatusReport.t option }
    let make ?region  ?i_p_address  ?status_report  () =
      { region; i_p_address; status_report }
    let parse xml =
      Some
        {
          region =
            (Util.option_bind (Xml.member "Region" xml)
               HealthCheckRegion.parse);
          i_p_address =
            (Util.option_bind (Xml.member "IPAddress" xml) String.parse);
          status_report =
            (Util.option_bind (Xml.member "StatusReport" xml)
               StatusReport.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status_report
              (fun f ->
                 Query.Pair ("StatusReport", (StatusReport.to_query f)));
           Util.option_map v.i_p_address
             (fun f -> Query.Pair ("IPAddress", (String.to_query f)));
           Util.option_map v.region
             (fun f -> Query.Pair ("Region", (HealthCheckRegion.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status_report
              (fun f -> ("status_report", (StatusReport.to_json f)));
           Util.option_map v.i_p_address
             (fun f -> ("i_p_address", (String.to_json f)));
           Util.option_map v.region
             (fun f -> ("region", (HealthCheckRegion.to_json f)))])
    let of_json j =
      {
        region =
          (Util.option_map (Json.lookup j "region") HealthCheckRegion.of_json);
        i_p_address =
          (Util.option_map (Json.lookup j "i_p_address") String.of_json);
        status_report =
          (Util.option_map (Json.lookup j "status_report")
             StatusReport.of_json)
      }
  end
module HostedZoneSummary =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      name: String.t ;
      owner: HostedZoneOwner.t }
    let make ~hosted_zone_id  ~name  ~owner  () =
      { hosted_zone_id; name; owner }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          owner =
            (Xml.required "Owner"
               (Util.option_bind (Xml.member "Owner" xml)
                  HostedZoneOwner.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Owner", (HostedZoneOwner.to_query v.owner)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("owner", (HostedZoneOwner.to_json v.owner));
           Some ("name", (String.to_json v.name));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        owner =
          (HostedZoneOwner.of_json
             (Util.of_option_exn (Json.lookup j "owner")))
      }
  end
module TrafficPolicyInstance =
  struct
    type t =
      {
      id: String.t ;
      hosted_zone_id: String.t ;
      name: String.t ;
      t_t_l: Long.t ;
      state: String.t ;
      message: String.t ;
      traffic_policy_id: String.t ;
      traffic_policy_version: Integer.t ;
      traffic_policy_type: RRType.t }
    let make ~id  ~hosted_zone_id  ~name  ~t_t_l  ~state  ~message 
      ~traffic_policy_id  ~traffic_policy_version  ~traffic_policy_type  () =
      {
        id;
        hosted_zone_id;
        name;
        t_t_l;
        state;
        message;
        traffic_policy_id;
        traffic_policy_version;
        traffic_policy_type
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          t_t_l =
            (Xml.required "TTL"
               (Util.option_bind (Xml.member "TTL" xml) Long.parse));
          state =
            (Xml.required "State"
               (Util.option_bind (Xml.member "State" xml) String.parse));
          message =
            (Xml.required "Message"
               (Util.option_bind (Xml.member "Message" xml) String.parse));
          traffic_policy_id =
            (Xml.required "TrafficPolicyId"
               (Util.option_bind (Xml.member "TrafficPolicyId" xml)
                  String.parse));
          traffic_policy_version =
            (Xml.required "TrafficPolicyVersion"
               (Util.option_bind (Xml.member "TrafficPolicyVersion" xml)
                  Integer.parse));
          traffic_policy_type =
            (Xml.required "TrafficPolicyType"
               (Util.option_bind (Xml.member "TrafficPolicyType" xml)
                  RRType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyType",
                   (RRType.to_query v.traffic_policy_type)));
           Some
             (Query.Pair
                ("TrafficPolicyVersion",
                  (Integer.to_query v.traffic_policy_version)));
           Some
             (Query.Pair
                ("TrafficPolicyId", (String.to_query v.traffic_policy_id)));
           Some (Query.Pair ("Message", (String.to_query v.message)));
           Some (Query.Pair ("State", (String.to_query v.state)));
           Some (Query.Pair ("TTL", (Long.to_query v.t_t_l)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_type", (RRType.to_json v.traffic_policy_type));
           Some
             ("traffic_policy_version",
               (Integer.to_json v.traffic_policy_version));
           Some ("traffic_policy_id", (String.to_json v.traffic_policy_id));
           Some ("message", (String.to_json v.message));
           Some ("state", (String.to_json v.state));
           Some ("t_t_l", (Long.to_json v.t_t_l));
           Some ("name", (String.to_json v.name));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        t_t_l = (Long.of_json (Util.of_option_exn (Json.lookup j "t_t_l")));
        state = (String.of_json (Util.of_option_exn (Json.lookup j "state")));
        message =
          (String.of_json (Util.of_option_exn (Json.lookup j "message")));
        traffic_policy_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_id")));
        traffic_policy_version =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_version")));
        traffic_policy_type =
          (RRType.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_type")))
      }
  end
module ResettableElementName =
  struct
    type t =
      | FullyQualifiedDomainName 
      | Regions 
      | ResourcePath 
      | ChildHealthChecks 
    let str_to_t =
      [("ChildHealthChecks", ChildHealthChecks);
      ("ResourcePath", ResourcePath);
      ("Regions", Regions);
      ("FullyQualifiedDomainName", FullyQualifiedDomainName)]
    let t_to_str =
      [(ChildHealthChecks, "ChildHealthChecks");
      (ResourcePath, "ResourcePath");
      (Regions, "Regions");
      (FullyQualifiedDomainName, "FullyQualifiedDomainName")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module VPC =
  struct
    type t = {
      v_p_c_region: VPCRegion.t option ;
      v_p_c_id: String.t option }
    let make ?v_p_c_region  ?v_p_c_id  () = { v_p_c_region; v_p_c_id }
    let parse xml =
      Some
        {
          v_p_c_region =
            (Util.option_bind (Xml.member "VPCRegion" xml) VPCRegion.parse);
          v_p_c_id = (Util.option_bind (Xml.member "VPCId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.v_p_c_id
              (fun f -> Query.Pair ("VPCId", (String.to_query f)));
           Util.option_map v.v_p_c_region
             (fun f -> Query.Pair ("VPCRegion", (VPCRegion.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.v_p_c_id
              (fun f -> ("v_p_c_id", (String.to_json f)));
           Util.option_map v.v_p_c_region
             (fun f -> ("v_p_c_region", (VPCRegion.to_json f)))])
    let of_json j =
      {
        v_p_c_region =
          (Util.option_map (Json.lookup j "v_p_c_region") VPCRegion.of_json);
        v_p_c_id =
          (Util.option_map (Json.lookup j "v_p_c_id") String.of_json)
      }
  end
module GeoLocationDetails =
  struct
    type t =
      {
      continent_code: String.t option ;
      continent_name: String.t option ;
      country_code: String.t option ;
      country_name: String.t option ;
      subdivision_code: String.t option ;
      subdivision_name: String.t option }
    let make ?continent_code  ?continent_name  ?country_code  ?country_name 
      ?subdivision_code  ?subdivision_name  () =
      {
        continent_code;
        continent_name;
        country_code;
        country_name;
        subdivision_code;
        subdivision_name
      }
    let parse xml =
      Some
        {
          continent_code =
            (Util.option_bind (Xml.member "ContinentCode" xml) String.parse);
          continent_name =
            (Util.option_bind (Xml.member "ContinentName" xml) String.parse);
          country_code =
            (Util.option_bind (Xml.member "CountryCode" xml) String.parse);
          country_name =
            (Util.option_bind (Xml.member "CountryName" xml) String.parse);
          subdivision_code =
            (Util.option_bind (Xml.member "SubdivisionCode" xml) String.parse);
          subdivision_name =
            (Util.option_bind (Xml.member "SubdivisionName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subdivision_name
              (fun f -> Query.Pair ("SubdivisionName", (String.to_query f)));
           Util.option_map v.subdivision_code
             (fun f -> Query.Pair ("SubdivisionCode", (String.to_query f)));
           Util.option_map v.country_name
             (fun f -> Query.Pair ("CountryName", (String.to_query f)));
           Util.option_map v.country_code
             (fun f -> Query.Pair ("CountryCode", (String.to_query f)));
           Util.option_map v.continent_name
             (fun f -> Query.Pair ("ContinentName", (String.to_query f)));
           Util.option_map v.continent_code
             (fun f -> Query.Pair ("ContinentCode", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subdivision_name
              (fun f -> ("subdivision_name", (String.to_json f)));
           Util.option_map v.subdivision_code
             (fun f -> ("subdivision_code", (String.to_json f)));
           Util.option_map v.country_name
             (fun f -> ("country_name", (String.to_json f)));
           Util.option_map v.country_code
             (fun f -> ("country_code", (String.to_json f)));
           Util.option_map v.continent_name
             (fun f -> ("continent_name", (String.to_json f)));
           Util.option_map v.continent_code
             (fun f -> ("continent_code", (String.to_json f)))])
    let of_json j =
      {
        continent_code =
          (Util.option_map (Json.lookup j "continent_code") String.of_json);
        continent_name =
          (Util.option_map (Json.lookup j "continent_name") String.of_json);
        country_code =
          (Util.option_map (Json.lookup j "country_code") String.of_json);
        country_name =
          (Util.option_map (Json.lookup j "country_name") String.of_json);
        subdivision_code =
          (Util.option_map (Json.lookup j "subdivision_code") String.of_json);
        subdivision_name =
          (Util.option_map (Json.lookup j "subdivision_name") String.of_json)
      }
  end
module HostedZoneLimitType =
  struct
    type t =
      | MAX_RRSETS_BY_ZONE 
      | MAX_VPCS_ASSOCIATED_BY_ZONE 
    let str_to_t =
      [("MAX_VPCS_ASSOCIATED_BY_ZONE", MAX_VPCS_ASSOCIATED_BY_ZONE);
      ("MAX_RRSETS_BY_ZONE", MAX_RRSETS_BY_ZONE)]
    let t_to_str =
      [(MAX_VPCS_ASSOCIATED_BY_ZONE, "MAX_VPCS_ASSOCIATED_BY_ZONE");
      (MAX_RRSETS_BY_ZONE, "MAX_RRSETS_BY_ZONE")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module HealthCheck =
  struct
    type t =
      {
      id: String.t ;
      caller_reference: String.t ;
      linked_service: LinkedService.t option ;
      health_check_config: HealthCheckConfig.t ;
      health_check_version: Long.t ;
      cloud_watch_alarm_configuration: CloudWatchAlarmConfiguration.t option }
    let make ~id  ~caller_reference  ?linked_service  ~health_check_config 
      ~health_check_version  ?cloud_watch_alarm_configuration  () =
      {
        id;
        caller_reference;
        linked_service;
        health_check_config;
        health_check_version;
        cloud_watch_alarm_configuration
      }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          linked_service =
            (Util.option_bind (Xml.member "LinkedService" xml)
               LinkedService.parse);
          health_check_config =
            (Xml.required "HealthCheckConfig"
               (Util.option_bind (Xml.member "HealthCheckConfig" xml)
                  HealthCheckConfig.parse));
          health_check_version =
            (Xml.required "HealthCheckVersion"
               (Util.option_bind (Xml.member "HealthCheckVersion" xml)
                  Long.parse));
          cloud_watch_alarm_configuration =
            (Util.option_bind (Xml.member "CloudWatchAlarmConfiguration" xml)
               CloudWatchAlarmConfiguration.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_alarm_configuration
              (fun f ->
                 Query.Pair
                   ("CloudWatchAlarmConfiguration",
                     (CloudWatchAlarmConfiguration.to_query f)));
           Some
             (Query.Pair
                ("HealthCheckVersion",
                  (Long.to_query v.health_check_version)));
           Some
             (Query.Pair
                ("HealthCheckConfig",
                  (HealthCheckConfig.to_query v.health_check_config)));
           Util.option_map v.linked_service
             (fun f ->
                Query.Pair ("LinkedService", (LinkedService.to_query f)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cloud_watch_alarm_configuration
              (fun f ->
                 ("cloud_watch_alarm_configuration",
                   (CloudWatchAlarmConfiguration.to_json f)));
           Some
             ("health_check_version", (Long.to_json v.health_check_version));
           Some
             ("health_check_config",
               (HealthCheckConfig.to_json v.health_check_config));
           Util.option_map v.linked_service
             (fun f -> ("linked_service", (LinkedService.to_json f)));
           Some ("caller_reference", (String.to_json v.caller_reference));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        linked_service =
          (Util.option_map (Json.lookup j "linked_service")
             LinkedService.of_json);
        health_check_config =
          (HealthCheckConfig.of_json
             (Util.of_option_exn (Json.lookup j "health_check_config")));
        health_check_version =
          (Long.of_json
             (Util.of_option_exn (Json.lookup j "health_check_version")));
        cloud_watch_alarm_configuration =
          (Util.option_map (Json.lookup j "cloud_watch_alarm_configuration")
             CloudWatchAlarmConfiguration.of_json)
      }
  end
module QueryLoggingConfig =
  struct
    type t =
      {
      id: String.t ;
      hosted_zone_id: String.t ;
      cloud_watch_logs_log_group_arn: String.t }
    let make ~id  ~hosted_zone_id  ~cloud_watch_logs_log_group_arn  () =
      { id; hosted_zone_id; cloud_watch_logs_log_group_arn }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          cloud_watch_logs_log_group_arn =
            (Xml.required "CloudWatchLogsLogGroupArn"
               (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CloudWatchLogsLogGroupArn",
                   (String.to_query v.cloud_watch_logs_log_group_arn)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cloud_watch_logs_log_group_arn",
                (String.to_json v.cloud_watch_logs_log_group_arn));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        cloud_watch_logs_log_group_arn =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "cloud_watch_logs_log_group_arn")))
      }
  end
module ResourceTagSet =
  struct
    type t =
      {
      resource_type: TagResourceType.t option ;
      resource_id: String.t option ;
      tags: TagList.t }
    let make ?resource_type  ?resource_id  ?(tags= [])  () =
      { resource_type; resource_id; tags }
    let parse xml =
      Some
        {
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml)
               TagResourceType.parse);
          resource_id =
            (Util.option_bind (Xml.member "ResourceId" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.resource_id
             (fun f -> Query.Pair ("ResourceId", (String.to_query f)));
           Util.option_map v.resource_type
             (fun f ->
                Query.Pair ("ResourceType", (TagResourceType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.resource_id
             (fun f -> ("resource_id", (String.to_json f)));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (TagResourceType.to_json f)))])
    let of_json j =
      {
        resource_type =
          (Util.option_map (Json.lookup j "resource_type")
             TagResourceType.of_json);
        resource_id =
          (Util.option_map (Json.lookup j "resource_id") String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module AccountLimitType =
  struct
    type t =
      | MAX_HEALTH_CHECKS_BY_OWNER 
      | MAX_HOSTED_ZONES_BY_OWNER 
      | MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER 
      | MAX_REUSABLE_DELEGATION_SETS_BY_OWNER 
      | MAX_TRAFFIC_POLICIES_BY_OWNER 
    let str_to_t =
      [("MAX_TRAFFIC_POLICIES_BY_OWNER", MAX_TRAFFIC_POLICIES_BY_OWNER);
      ("MAX_REUSABLE_DELEGATION_SETS_BY_OWNER",
        MAX_REUSABLE_DELEGATION_SETS_BY_OWNER);
      ("MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER",
        MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER);
      ("MAX_HOSTED_ZONES_BY_OWNER", MAX_HOSTED_ZONES_BY_OWNER);
      ("MAX_HEALTH_CHECKS_BY_OWNER", MAX_HEALTH_CHECKS_BY_OWNER)]
    let t_to_str =
      [(MAX_TRAFFIC_POLICIES_BY_OWNER, "MAX_TRAFFIC_POLICIES_BY_OWNER");
      (MAX_REUSABLE_DELEGATION_SETS_BY_OWNER,
        "MAX_REUSABLE_DELEGATION_SETS_BY_OWNER");
      (MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER,
        "MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER");
      (MAX_HOSTED_ZONES_BY_OWNER, "MAX_HOSTED_ZONES_BY_OWNER");
      (MAX_HEALTH_CHECKS_BY_OWNER, "MAX_HEALTH_CHECKS_BY_OWNER")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ReusableDelegationSetLimitType =
  struct
    type t =
      | MAX_ZONES_BY_REUSABLE_DELEGATION_SET 
    let str_to_t =
      [("MAX_ZONES_BY_REUSABLE_DELEGATION_SET",
         MAX_ZONES_BY_REUSABLE_DELEGATION_SET)]
    let t_to_str =
      [(MAX_ZONES_BY_REUSABLE_DELEGATION_SET,
         "MAX_ZONES_BY_REUSABLE_DELEGATION_SET")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module DelegationSet =
  struct
    type t =
      {
      id: String.t option ;
      caller_reference: String.t option ;
      name_servers: DelegationSetNameServers.t }
    let make ?id  ?caller_reference  ~name_servers  () =
      { id; caller_reference; name_servers }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          caller_reference =
            (Util.option_bind (Xml.member "CallerReference" xml) String.parse);
          name_servers =
            (Xml.required "NameServers"
               (Util.option_bind (Xml.member "NameServers" xml)
                  DelegationSetNameServers.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NameServers.member",
                   (DelegationSetNameServers.to_query v.name_servers)));
           Util.option_map v.caller_reference
             (fun f -> Query.Pair ("CallerReference", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("name_servers",
                (DelegationSetNameServers.to_json v.name_servers));
           Util.option_map v.caller_reference
             (fun f -> ("caller_reference", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        caller_reference =
          (Util.option_map (Json.lookup j "caller_reference") String.of_json);
        name_servers =
          (DelegationSetNameServers.of_json
             (Util.of_option_exn (Json.lookup j "name_servers")))
      }
  end
module TrafficPolicySummary =
  struct
    type t =
      {
      id: String.t ;
      name: String.t ;
      type_: RRType.t ;
      latest_version: Integer.t ;
      traffic_policy_count: Integer.t }
    let make ~id  ~name  ~type_  ~latest_version  ~traffic_policy_count  () =
      { id; name; type_; latest_version; traffic_policy_count }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml) RRType.parse));
          latest_version =
            (Xml.required "LatestVersion"
               (Util.option_bind (Xml.member "LatestVersion" xml)
                  Integer.parse));
          traffic_policy_count =
            (Xml.required "TrafficPolicyCount"
               (Util.option_bind (Xml.member "TrafficPolicyCount" xml)
                  Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyCount",
                   (Integer.to_query v.traffic_policy_count)));
           Some
             (Query.Pair
                ("LatestVersion", (Integer.to_query v.latest_version)));
           Some (Query.Pair ("Type", (RRType.to_query v.type_)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_count",
                (Integer.to_json v.traffic_policy_count));
           Some ("latest_version", (Integer.to_json v.latest_version));
           Some ("type_", (RRType.to_json v.type_));
           Some ("name", (String.to_json v.name));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        type_ = (RRType.of_json (Util.of_option_exn (Json.lookup j "type_")));
        latest_version =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "latest_version")));
        traffic_policy_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_count")))
      }
  end
module Changes =
  struct
    type t = Change.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Change.parse (Xml.members "Change" xml))
    let to_query v = Query.to_query_list Change.to_query v
    let to_json v = `List (List.map Change.to_json v)
    let of_json j = Json.to_list Change.of_json j
  end
module HostedZones =
  struct
    type t = HostedZone.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map HostedZone.parse (Xml.members "HostedZone" xml))
    let to_query v = Query.to_query_list HostedZone.to_query v
    let to_json v = `List (List.map HostedZone.to_json v)
    let of_json j = Json.to_list HostedZone.of_json j
  end
module ChangeInfo =
  struct
    type t =
      {
      id: String.t ;
      status: ChangeStatus.t ;
      submitted_at: DateTime.t ;
      comment: String.t option }
    let make ~id  ~status  ~submitted_at  ?comment  () =
      { id; status; submitted_at; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml) ChangeStatus.parse));
          submitted_at =
            (Xml.required "SubmittedAt"
               (Util.option_bind (Xml.member "SubmittedAt" xml)
                  DateTime.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some
             (Query.Pair ("SubmittedAt", (DateTime.to_query v.submitted_at)));
           Some (Query.Pair ("Status", (ChangeStatus.to_query v.status)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("submitted_at", (DateTime.to_json v.submitted_at));
           Some ("status", (ChangeStatus.to_json v.status));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        status =
          (ChangeStatus.of_json (Util.of_option_exn (Json.lookup j "status")));
        submitted_at =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "submitted_at")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module TrafficPolicies =
  struct
    type t = TrafficPolicy.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TrafficPolicy.parse (Xml.members "TrafficPolicy" xml))
    let to_query v = Query.to_query_list TrafficPolicy.to_query v
    let to_json v = `List (List.map TrafficPolicy.to_json v)
    let of_json j = Json.to_list TrafficPolicy.of_json j
  end
module HealthCheckObservations =
  struct
    type t = HealthCheckObservation.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map HealthCheckObservation.parse
           (Xml.members "HealthCheckObservation" xml))
    let to_query v = Query.to_query_list HealthCheckObservation.to_query v
    let to_json v = `List (List.map HealthCheckObservation.to_json v)
    let of_json j = Json.to_list HealthCheckObservation.of_json j
  end
module CheckerIpRanges =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module HostedZoneSummaries =
  struct
    type t = HostedZoneSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map HostedZoneSummary.parse
           (Xml.members "HostedZoneSummary" xml))
    let to_query v = Query.to_query_list HostedZoneSummary.to_query v
    let to_json v = `List (List.map HostedZoneSummary.to_json v)
    let of_json j = Json.to_list HostedZoneSummary.of_json j
  end
module TrafficPolicyInstances =
  struct
    type t = TrafficPolicyInstance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TrafficPolicyInstance.parse
           (Xml.members "TrafficPolicyInstance" xml))
    let to_query v = Query.to_query_list TrafficPolicyInstance.to_query v
    let to_json v = `List (List.map TrafficPolicyInstance.to_json v)
    let of_json j = Json.to_list TrafficPolicyInstance.of_json j
  end
module ErrorMessages =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Message" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResettableElementNameList =
  struct
    type t = ResettableElementName.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResettableElementName.parse
           (Xml.members "ResettableElementName" xml))
    let to_query v = Query.to_query_list ResettableElementName.to_query v
    let to_json v = `List (List.map ResettableElementName.to_json v)
    let of_json j = Json.to_list ResettableElementName.of_json j
  end
module VPCs =
  struct
    type t = VPC.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map VPC.parse (Xml.members "VPC" xml))
    let to_query v = Query.to_query_list VPC.to_query v
    let to_json v = `List (List.map VPC.to_json v)
    let of_json j = Json.to_list VPC.of_json j
  end
module TagResourceIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "ResourceId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module GeoLocationDetailsList =
  struct
    type t = GeoLocationDetails.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GeoLocationDetails.parse
           (Xml.members "GeoLocationDetails" xml))
    let to_query v = Query.to_query_list GeoLocationDetails.to_query v
    let to_json v = `List (List.map GeoLocationDetails.to_json v)
    let of_json j = Json.to_list GeoLocationDetails.of_json j
  end
module HostedZoneLimit =
  struct
    type t = {
      type_: HostedZoneLimitType.t ;
      value: Long.t }
    let make ~type_  ~value  () = { type_; value }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  HostedZoneLimitType.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (Long.to_query v.value)));
           Some (Query.Pair ("Type", (HostedZoneLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (Long.to_json v.value));
           Some ("type_", (HostedZoneLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (HostedZoneLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        value = (Long.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module HealthChecks =
  struct
    type t = HealthCheck.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map HealthCheck.parse (Xml.members "HealthCheck" xml))
    let to_query v = Query.to_query_list HealthCheck.to_query v
    let to_json v = `List (List.map HealthCheck.to_json v)
    let of_json j = Json.to_list HealthCheck.of_json j
  end
module QueryLoggingConfigs =
  struct
    type t = QueryLoggingConfig.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map QueryLoggingConfig.parse
           (Xml.members "QueryLoggingConfig" xml))
    let to_query v = Query.to_query_list QueryLoggingConfig.to_query v
    let to_json v = `List (List.map QueryLoggingConfig.to_json v)
    let of_json j = Json.to_list QueryLoggingConfig.of_json j
  end
module TagKeyList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Key" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResourceTagSetList =
  struct
    type t = ResourceTagSet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceTagSet.parse (Xml.members "ResourceTagSet" xml))
    let to_query v = Query.to_query_list ResourceTagSet.to_query v
    let to_json v = `List (List.map ResourceTagSet.to_json v)
    let of_json j = Json.to_list ResourceTagSet.of_json j
  end
module AccountLimit =
  struct
    type t = {
      type_: AccountLimitType.t ;
      value: Long.t }
    let make ~type_  ~value  () = { type_; value }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  AccountLimitType.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (Long.to_query v.value)));
           Some (Query.Pair ("Type", (AccountLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (Long.to_json v.value));
           Some ("type_", (AccountLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (AccountLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        value = (Long.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module ResourceRecordSets =
  struct
    type t = ResourceRecordSet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceRecordSet.parse
           (Xml.members "ResourceRecordSet" xml))
    let to_query v = Query.to_query_list ResourceRecordSet.to_query v
    let to_json v = `List (List.map ResourceRecordSet.to_json v)
    let of_json j = Json.to_list ResourceRecordSet.of_json j
  end
module ReusableDelegationSetLimit =
  struct
    type t = {
      type_: ReusableDelegationSetLimitType.t ;
      value: Long.t }
    let make ~type_  ~value  () = { type_; value }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  ReusableDelegationSetLimitType.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (Long.to_query v.value)));
           Some
             (Query.Pair
                ("Type", (ReusableDelegationSetLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (Long.to_json v.value));
           Some ("type_", (ReusableDelegationSetLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (ReusableDelegationSetLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        value = (Long.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module DelegationSets =
  struct
    type t = DelegationSet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DelegationSet.parse (Xml.members "DelegationSet" xml))
    let to_query v = Query.to_query_list DelegationSet.to_query v
    let to_json v = `List (List.map DelegationSet.to_json v)
    let of_json j = Json.to_list DelegationSet.of_json j
  end
module TrafficPolicySummaries =
  struct
    type t = TrafficPolicySummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TrafficPolicySummary.parse
           (Xml.members "TrafficPolicySummary" xml))
    let to_query v = Query.to_query_list TrafficPolicySummary.to_query v
    let to_json v = `List (List.map TrafficPolicySummary.to_json v)
    let of_json j = Json.to_list TrafficPolicySummary.of_json j
  end
module ChangeBatch =
  struct
    type t = {
      comment: String.t option ;
      changes: Changes.t }
    let make ?comment  ~changes  () = { comment; changes }
    let parse xml =
      Some
        {
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse);
          changes =
            (Xml.required "Changes"
               (Util.option_bind (Xml.member "Changes" xml) Changes.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Changes.member", (Changes.to_query v.changes)));
           Util.option_map v.comment
             (fun f -> Query.Pair ("Comment", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("changes", (Changes.to_json v.changes));
           Util.option_map v.comment
             (fun f -> ("comment", (String.to_json f)))])
    let of_json j =
      {
        comment = (Util.option_map (Json.lookup j "comment") String.of_json);
        changes =
          (Changes.of_json (Util.of_option_exn (Json.lookup j "changes")))
      }
  end
module RecordData =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "RecordDataEntry" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ListHostedZonesByNameResponse =
  struct
    type t =
      {
      hosted_zones: HostedZones.t ;
      d_n_s_name: String.t option ;
      hosted_zone_id: String.t option ;
      is_truncated: Boolean.t ;
      next_d_n_s_name: String.t option ;
      next_hosted_zone_id: String.t option ;
      max_items: String.t }
    let make ~hosted_zones  ?d_n_s_name  ?hosted_zone_id  ~is_truncated 
      ?next_d_n_s_name  ?next_hosted_zone_id  ~max_items  () =
      {
        hosted_zones;
        d_n_s_name;
        hosted_zone_id;
        is_truncated;
        next_d_n_s_name;
        next_hosted_zone_id;
        max_items
      }
    let parse xml =
      Some
        {
          hosted_zones =
            (Xml.required "HostedZones"
               (Util.option_bind (Xml.member "HostedZones" xml)
                  HostedZones.parse));
          d_n_s_name =
            (Util.option_bind (Xml.member "DNSName" xml) String.parse);
          hosted_zone_id =
            (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse);
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_d_n_s_name =
            (Util.option_bind (Xml.member "NextDNSName" xml) String.parse);
          next_hosted_zone_id =
            (Util.option_bind (Xml.member "NextHostedZoneId" xml)
               String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_hosted_zone_id
             (fun f -> Query.Pair ("NextHostedZoneId", (String.to_query f)));
           Util.option_map v.next_d_n_s_name
             (fun f -> Query.Pair ("NextDNSName", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Util.option_map v.hosted_zone_id
             (fun f -> Query.Pair ("HostedZoneId", (String.to_query f)));
           Util.option_map v.d_n_s_name
             (fun f -> Query.Pair ("DNSName", (String.to_query f)));
           Some
             (Query.Pair
                ("HostedZones.member", (HostedZones.to_query v.hosted_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_hosted_zone_id
             (fun f -> ("next_hosted_zone_id", (String.to_json f)));
           Util.option_map v.next_d_n_s_name
             (fun f -> ("next_d_n_s_name", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Util.option_map v.hosted_zone_id
             (fun f -> ("hosted_zone_id", (String.to_json f)));
           Util.option_map v.d_n_s_name
             (fun f -> ("d_n_s_name", (String.to_json f)));
           Some ("hosted_zones", (HostedZones.to_json v.hosted_zones))])
    let of_json j =
      {
        hosted_zones =
          (HostedZones.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zones")));
        d_n_s_name =
          (Util.option_map (Json.lookup j "d_n_s_name") String.of_json);
        hosted_zone_id =
          (Util.option_map (Json.lookup j "hosted_zone_id") String.of_json);
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_d_n_s_name =
          (Util.option_map (Json.lookup j "next_d_n_s_name") String.of_json);
        next_hosted_zone_id =
          (Util.option_map (Json.lookup j "next_hosted_zone_id")
             String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module DeleteReusableDelegationSetResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module HostedZoneNotFound =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteQueryLoggingConfigResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetTrafficPolicyInstanceResponse =
  struct
    type t = {
      traffic_policy_instance: TrafficPolicyInstance.t }
    let make ~traffic_policy_instance  () = { traffic_policy_instance }
    let parse xml =
      Some
        {
          traffic_policy_instance =
            (Xml.required "TrafficPolicyInstance"
               (Util.option_bind (Xml.member "TrafficPolicyInstance" xml)
                  TrafficPolicyInstance.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyInstance",
                   (TrafficPolicyInstance.to_query v.traffic_policy_instance)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_instance",
                (TrafficPolicyInstance.to_json v.traffic_policy_instance))])
    let of_json j =
      {
        traffic_policy_instance =
          (TrafficPolicyInstance.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instance")))
      }
  end
module ListHealthChecksRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: String.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module AssociateVPCWithHostedZoneResponse =
  struct
    type t = {
      change_info: ChangeInfo.t }
    let make ~change_info  () = { change_info }
    let parse xml =
      Some
        {
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_info", (ChangeInfo.to_json v.change_info))])
    let of_json j =
      {
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")))
      }
  end
module ListTrafficPolicyVersionsResponse =
  struct
    type t =
      {
      traffic_policies: TrafficPolicies.t ;
      is_truncated: Boolean.t ;
      traffic_policy_version_marker: String.t ;
      max_items: String.t }
    let make ~traffic_policies  ~is_truncated  ~traffic_policy_version_marker
       ~max_items  () =
      {
        traffic_policies;
        is_truncated;
        traffic_policy_version_marker;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policies =
            (Xml.required "TrafficPolicies"
               (Util.option_bind (Xml.member "TrafficPolicies" xml)
                  TrafficPolicies.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          traffic_policy_version_marker =
            (Xml.required "TrafficPolicyVersionMarker"
               (Util.option_bind
                  (Xml.member "TrafficPolicyVersionMarker" xml) String.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair
                ("TrafficPolicyVersionMarker",
                  (String.to_query v.traffic_policy_version_marker)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some
             (Query.Pair
                ("TrafficPolicies.member",
                  (TrafficPolicies.to_query v.traffic_policies)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Some
             ("traffic_policy_version_marker",
               (String.to_json v.traffic_policy_version_marker));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some
             ("traffic_policies",
               (TrafficPolicies.to_json v.traffic_policies))])
    let of_json j =
      {
        traffic_policies =
          (TrafficPolicies.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policies")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        traffic_policy_version_marker =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "traffic_policy_version_marker")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module DelegationSetAlreadyReusable =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetChangeRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module GetGeoLocationResponse =
  struct
    type t = {
      geo_location_details: GeoLocationDetails.t }
    let make ~geo_location_details  () = { geo_location_details }
    let parse xml =
      Some
        {
          geo_location_details =
            (Xml.required "GeoLocationDetails"
               (Util.option_bind (Xml.member "GeoLocationDetails" xml)
                  GeoLocationDetails.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GeoLocationDetails",
                   (GeoLocationDetails.to_query v.geo_location_details)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("geo_location_details",
                (GeoLocationDetails.to_json v.geo_location_details))])
    let of_json j =
      {
        geo_location_details =
          (GeoLocationDetails.of_json
             (Util.of_option_exn (Json.lookup j "geo_location_details")))
      }
  end
module GetHealthCheckLastFailureReasonResponse =
  struct
    type t = {
      health_check_observations: HealthCheckObservations.t }
    let make ~health_check_observations  () = { health_check_observations }
    let parse xml =
      Some
        {
          health_check_observations =
            (Xml.required "HealthCheckObservations"
               (Util.option_bind (Xml.member "HealthCheckObservations" xml)
                  HealthCheckObservations.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckObservations.member",
                   (HealthCheckObservations.to_query
                      v.health_check_observations)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("health_check_observations",
                (HealthCheckObservations.to_json v.health_check_observations))])
    let of_json j =
      {
        health_check_observations =
          (HealthCheckObservations.of_json
             (Util.of_option_exn (Json.lookup j "health_check_observations")))
      }
  end
module VPCAssociationAuthorizationNotFound =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteVPCAssociationAuthorizationRequest =
  struct
    type t = {
      hosted_zone_id: String.t ;
      v_p_c: VPC.t }
    let make ~hosted_zone_id  ~v_p_c  () = { hosted_zone_id; v_p_c }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          v_p_c =
            (Xml.required "VPC"
               (Util.option_bind (Xml.member "VPC" xml) VPC.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("VPC", (VPC.to_query v.v_p_c)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("v_p_c", (VPC.to_json v.v_p_c));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        v_p_c = (VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c")))
      }
  end
module TooManyTrafficPolicyInstances =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetHostedZoneRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module DelegationSetAlreadyCreated =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetAccountLimitRequest =
  struct
    type t = {
      type_: AccountLimitType.t }
    let make ~type_  () = { type_ }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  AccountLimitType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Type", (AccountLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("type_", (AccountLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (AccountLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")))
      }
  end
module GetTrafficPolicyInstanceRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module ListReusableDelegationSetsRequest =
  struct
    type t = {
      marker: String.t option ;
      max_items: String.t option }
    let make ?marker  ?max_items  () = { marker; max_items }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module DeleteTrafficPolicyInstanceResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidArgument =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateHostedZoneResponse =
  struct
    type t =
      {
      hosted_zone: HostedZone.t ;
      change_info: ChangeInfo.t ;
      delegation_set: DelegationSet.t ;
      v_p_c: VPC.t option ;
      location: String.t }
    let make ~hosted_zone  ~change_info  ~delegation_set  ?v_p_c  ~location 
      () = { hosted_zone; change_info; delegation_set; v_p_c; location }
    let parse xml =
      Some
        {
          hosted_zone =
            (Xml.required "HostedZone"
               (Util.option_bind (Xml.member "HostedZone" xml)
                  HostedZone.parse));
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse));
          delegation_set =
            (Xml.required "DelegationSet"
               (Util.option_bind (Xml.member "DelegationSet" xml)
                  DelegationSet.parse));
          v_p_c = (Util.option_bind (Xml.member "VPC" xml) VPC.parse);
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Util.option_map v.v_p_c
             (fun f -> Query.Pair ("VPC", (VPC.to_query f)));
           Some
             (Query.Pair
                ("DelegationSet", (DelegationSet.to_query v.delegation_set)));
           Some
             (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)));
           Some
             (Query.Pair ("HostedZone", (HostedZone.to_query v.hosted_zone)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Util.option_map v.v_p_c (fun f -> ("v_p_c", (VPC.to_json f)));
           Some ("delegation_set", (DelegationSet.to_json v.delegation_set));
           Some ("change_info", (ChangeInfo.to_json v.change_info));
           Some ("hosted_zone", (HostedZone.to_json v.hosted_zone))])
    let of_json j =
      {
        hosted_zone =
          (HostedZone.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone")));
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")));
        delegation_set =
          (DelegationSet.of_json
             (Util.of_option_exn (Json.lookup j "delegation_set")));
        v_p_c = (Util.option_map (Json.lookup j "v_p_c") VPC.of_json);
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module GetHostedZoneLimitRequest =
  struct
    type t = {
      type_: HostedZoneLimitType.t ;
      hosted_zone_id: String.t }
    let make ~type_  ~hosted_zone_id  () = { type_; hosted_zone_id }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  HostedZoneLimitType.parse));
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)));
           Some (Query.Pair ("Type", (HostedZoneLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("hosted_zone_id", (String.to_json v.hosted_zone_id));
           Some ("type_", (HostedZoneLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (HostedZoneLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")))
      }
  end
module GetQueryLoggingConfigResponse =
  struct
    type t = {
      query_logging_config: QueryLoggingConfig.t }
    let make ~query_logging_config  () = { query_logging_config }
    let parse xml =
      Some
        {
          query_logging_config =
            (Xml.required "QueryLoggingConfig"
               (Util.option_bind (Xml.member "QueryLoggingConfig" xml)
                  QueryLoggingConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("QueryLoggingConfig",
                   (QueryLoggingConfig.to_query v.query_logging_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("query_logging_config",
                (QueryLoggingConfig.to_json v.query_logging_config))])
    let of_json j =
      {
        query_logging_config =
          (QueryLoggingConfig.of_json
             (Util.of_option_exn (Json.lookup j "query_logging_config")))
      }
  end
module GetHealthCheckStatusResponse =
  struct
    type t = {
      health_check_observations: HealthCheckObservations.t }
    let make ~health_check_observations  () = { health_check_observations }
    let parse xml =
      Some
        {
          health_check_observations =
            (Xml.required "HealthCheckObservations"
               (Util.option_bind (Xml.member "HealthCheckObservations" xml)
                  HealthCheckObservations.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckObservations.member",
                   (HealthCheckObservations.to_query
                      v.health_check_observations)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("health_check_observations",
                (HealthCheckObservations.to_json v.health_check_observations))])
    let of_json j =
      {
        health_check_observations =
          (HealthCheckObservations.of_json
             (Util.of_option_exn (Json.lookup j "health_check_observations")))
      }
  end
module GetCheckerIpRangesResponse =
  struct
    type t = {
      checker_ip_ranges: CheckerIpRanges.t }
    let make ~checker_ip_ranges  () = { checker_ip_ranges }
    let parse xml =
      Some
        {
          checker_ip_ranges =
            (Xml.required "CheckerIpRanges"
               (Util.option_bind (Xml.member "CheckerIpRanges" xml)
                  CheckerIpRanges.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CheckerIpRanges.member",
                   (CheckerIpRanges.to_query v.checker_ip_ranges)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("checker_ip_ranges",
                (CheckerIpRanges.to_json v.checker_ip_ranges))])
    let of_json j =
      {
        checker_ip_ranges =
          (CheckerIpRanges.of_json
             (Util.of_option_exn (Json.lookup j "checker_ip_ranges")))
      }
  end
module ListHostedZonesByVPCResponse =
  struct
    type t =
      {
      hosted_zone_summaries: HostedZoneSummaries.t ;
      max_items: String.t ;
      next_token: String.t option }
    let make ~hosted_zone_summaries  ~max_items  ?next_token  () =
      { hosted_zone_summaries; max_items; next_token }
    let parse xml =
      Some
        {
          hosted_zone_summaries =
            (Xml.required "HostedZoneSummaries"
               (Util.option_bind (Xml.member "HostedZoneSummaries" xml)
                  HostedZoneSummaries.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair
                ("HostedZoneSummaries.member",
                  (HostedZoneSummaries.to_query v.hosted_zone_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("max_items", (String.to_json v.max_items));
           Some
             ("hosted_zone_summaries",
               (HostedZoneSummaries.to_json v.hosted_zone_summaries))])
    let of_json j =
      {
        hosted_zone_summaries =
          (HostedZoneSummaries.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_summaries")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTrafficPolicyInstancesResponse =
  struct
    type t =
      {
      traffic_policy_instances: TrafficPolicyInstances.t ;
      hosted_zone_id_marker: String.t option ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      is_truncated: Boolean.t ;
      max_items: String.t }
    let make ~traffic_policy_instances  ?hosted_zone_id_marker 
      ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ~is_truncated  ~max_items  () =
      {
        traffic_policy_instances;
        hosted_zone_id_marker;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        is_truncated;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policy_instances =
            (Xml.required "TrafficPolicyInstances"
               (Util.option_bind (Xml.member "TrafficPolicyInstances" xml)
                  TrafficPolicyInstances.parse));
          hosted_zone_id_marker =
            (Util.option_bind (Xml.member "HostedZoneIdMarker" xml)
               String.parse);
          traffic_policy_instance_name_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceNameMarker" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceTypeMarker" xml)
               RRType.parse);
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceTypeMarker", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceNameMarker", (String.to_query f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> Query.Pair ("HostedZoneIdMarker", (String.to_query f)));
           Some
             (Query.Pair
                ("TrafficPolicyInstances.member",
                  (TrafficPolicyInstances.to_query v.traffic_policy_instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> ("hosted_zone_id_marker", (String.to_json f)));
           Some
             ("traffic_policy_instances",
               (TrafficPolicyInstances.to_json v.traffic_policy_instances))])
    let of_json j =
      {
        traffic_policy_instances =
          (TrafficPolicyInstances.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instances")));
        hosted_zone_id_marker =
          (Util.option_map (Json.lookup j "hosted_zone_id_marker")
             String.of_json);
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module ThrottlingException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidInput =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module NoSuchDelegationSet =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteTrafficPolicyRequest =
  struct
    type t = {
      id: String.t ;
      version: Integer.t }
    let make ~id  ~version  () = { id; version }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          version =
            (Xml.required "Version"
               (Util.option_bind (Xml.member "Version" xml) Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Version", (Integer.to_query v.version)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("version", (Integer.to_json v.version));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        version =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "version")))
      }
  end
module GetHealthCheckResponse =
  struct
    type t = {
      health_check: HealthCheck.t }
    let make ~health_check  () = { health_check }
    let parse xml =
      Some
        {
          health_check =
            (Xml.required "HealthCheck"
               (Util.option_bind (Xml.member "HealthCheck" xml)
                  HealthCheck.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheck", (HealthCheck.to_query v.health_check)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check", (HealthCheck.to_json v.health_check))])
    let of_json j =
      {
        health_check =
          (HealthCheck.of_json
             (Util.of_option_exn (Json.lookup j "health_check")))
      }
  end
module GetHealthCheckStatusRequest =
  struct
    type t = {
      health_check_id: String.t }
    let make ~health_check_id  () = { health_check_id }
    let parse xml =
      Some
        {
          health_check_id =
            (Xml.required "HealthCheckId"
               (Util.option_bind (Xml.member "HealthCheckId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckId", (String.to_query v.health_check_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check_id", (String.to_json v.health_check_id))])
    let of_json j =
      {
        health_check_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "health_check_id")))
      }
  end
module CreateTrafficPolicyInstanceRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      name: String.t ;
      t_t_l: Long.t ;
      traffic_policy_id: String.t ;
      traffic_policy_version: Integer.t }
    let make ~hosted_zone_id  ~name  ~t_t_l  ~traffic_policy_id 
      ~traffic_policy_version  () =
      {
        hosted_zone_id;
        name;
        t_t_l;
        traffic_policy_id;
        traffic_policy_version
      }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          t_t_l =
            (Xml.required "TTL"
               (Util.option_bind (Xml.member "TTL" xml) Long.parse));
          traffic_policy_id =
            (Xml.required "TrafficPolicyId"
               (Util.option_bind (Xml.member "TrafficPolicyId" xml)
                  String.parse));
          traffic_policy_version =
            (Xml.required "TrafficPolicyVersion"
               (Util.option_bind (Xml.member "TrafficPolicyVersion" xml)
                  Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyVersion",
                   (Integer.to_query v.traffic_policy_version)));
           Some
             (Query.Pair
                ("TrafficPolicyId", (String.to_query v.traffic_policy_id)));
           Some (Query.Pair ("TTL", (Long.to_query v.t_t_l)));
           Some (Query.Pair ("Name", (String.to_query v.name)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_version",
                (Integer.to_json v.traffic_policy_version));
           Some ("traffic_policy_id", (String.to_json v.traffic_policy_id));
           Some ("t_t_l", (Long.to_json v.t_t_l));
           Some ("name", (String.to_json v.name));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        t_t_l = (Long.of_json (Util.of_option_exn (Json.lookup j "t_t_l")));
        traffic_policy_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_id")));
        traffic_policy_version =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_version")))
      }
  end
module ListVPCAssociationAuthorizationsRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      next_token: String.t option ;
      max_results: String.t option }
    let make ~hosted_zone_id  ?next_token  ?max_results  () =
      { hosted_zone_id; next_token; max_results }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "nexttoken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "maxresults" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("maxresults", (String.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("nexttoken", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (String.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") String.of_json)
      }
  end
module ListTagsForResourceRequest =
  struct
    type t = {
      resource_type: TagResourceType.t ;
      resource_id: String.t }
    let make ~resource_type  ~resource_id  () =
      { resource_type; resource_id }
    let parse xml =
      Some
        {
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml)
                  TagResourceType.parse));
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)));
           Some
             (Query.Pair
                ("ResourceType", (TagResourceType.to_query v.resource_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("resource_id", (String.to_json v.resource_id));
           Some ("resource_type", (TagResourceType.to_json v.resource_type))])
    let of_json j =
      {
        resource_type =
          (TagResourceType.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")))
      }
  end
module HostedZoneNotEmpty =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetCheckerIpRangesRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetHealthCheckLastFailureReasonRequest =
  struct
    type t = {
      health_check_id: String.t }
    let make ~health_check_id  () = { health_check_id }
    let parse xml =
      Some
        {
          health_check_id =
            (Xml.required "HealthCheckId"
               (Util.option_bind (Xml.member "HealthCheckId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckId", (String.to_query v.health_check_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check_id", (String.to_json v.health_check_id))])
    let of_json j =
      {
        health_check_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "health_check_id")))
      }
  end
module GetTrafficPolicyResponse =
  struct
    type t = {
      traffic_policy: TrafficPolicy.t }
    let make ~traffic_policy  () = { traffic_policy }
    let parse xml =
      Some
        {
          traffic_policy =
            (Xml.required "TrafficPolicy"
               (Util.option_bind (Xml.member "TrafficPolicy" xml)
                  TrafficPolicy.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicy", (TrafficPolicy.to_query v.traffic_policy)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("traffic_policy", (TrafficPolicy.to_json v.traffic_policy))])
    let of_json j =
      {
        traffic_policy =
          (TrafficPolicy.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy")))
      }
  end
module LimitsExceeded =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module PublicZoneVPCAssociation =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyTrafficPolicyVersionsForCurrentPolicy =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module NoSuchTrafficPolicyInstance =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetHealthCheckRequest =
  struct
    type t = {
      health_check_id: String.t }
    let make ~health_check_id  () = { health_check_id }
    let parse xml =
      Some
        {
          health_check_id =
            (Xml.required "HealthCheckId"
               (Util.option_bind (Xml.member "HealthCheckId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckId", (String.to_query v.health_check_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check_id", (String.to_json v.health_check_id))])
    let of_json j =
      {
        health_check_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "health_check_id")))
      }
  end
module InvalidPaginationToken =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListResourceRecordSetsRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      start_record_name: String.t option ;
      start_record_type: RRType.t option ;
      start_record_identifier: String.t option ;
      max_items: String.t option }
    let make ~hosted_zone_id  ?start_record_name  ?start_record_type 
      ?start_record_identifier  ?max_items  () =
      {
        hosted_zone_id;
        start_record_name;
        start_record_type;
        start_record_identifier;
        max_items
      }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          start_record_name =
            (Util.option_bind (Xml.member "name" xml) String.parse);
          start_record_type =
            (Util.option_bind (Xml.member "type" xml) RRType.parse);
          start_record_identifier =
            (Util.option_bind (Xml.member "identifier" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.start_record_identifier
             (fun f -> Query.Pair ("identifier", (String.to_query f)));
           Util.option_map v.start_record_type
             (fun f -> Query.Pair ("type", (RRType.to_query f)));
           Util.option_map v.start_record_name
             (fun f -> Query.Pair ("name", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.start_record_identifier
             (fun f -> ("start_record_identifier", (String.to_json f)));
           Util.option_map v.start_record_type
             (fun f -> ("start_record_type", (RRType.to_json f)));
           Util.option_map v.start_record_name
             (fun f -> ("start_record_name", (String.to_json f)));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        start_record_name =
          (Util.option_map (Json.lookup j "start_record_name") String.of_json);
        start_record_type =
          (Util.option_map (Json.lookup j "start_record_type") RRType.of_json);
        start_record_identifier =
          (Util.option_map (Json.lookup j "start_record_identifier")
             String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module TestDNSAnswerRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      record_name: String.t ;
      record_type: RRType.t ;
      resolver_i_p: String.t option ;
      e_d_n_s0_client_subnet_i_p: String.t option ;
      e_d_n_s0_client_subnet_mask: String.t option }
    let make ~hosted_zone_id  ~record_name  ~record_type  ?resolver_i_p 
      ?e_d_n_s0_client_subnet_i_p  ?e_d_n_s0_client_subnet_mask  () =
      {
        hosted_zone_id;
        record_name;
        record_type;
        resolver_i_p;
        e_d_n_s0_client_subnet_i_p;
        e_d_n_s0_client_subnet_mask
      }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "hostedzoneid"
               (Util.option_bind (Xml.member "hostedzoneid" xml) String.parse));
          record_name =
            (Xml.required "recordname"
               (Util.option_bind (Xml.member "recordname" xml) String.parse));
          record_type =
            (Xml.required "recordtype"
               (Util.option_bind (Xml.member "recordtype" xml) RRType.parse));
          resolver_i_p =
            (Util.option_bind (Xml.member "resolverip" xml) String.parse);
          e_d_n_s0_client_subnet_i_p =
            (Util.option_bind (Xml.member "edns0clientsubnetip" xml)
               String.parse);
          e_d_n_s0_client_subnet_mask =
            (Util.option_bind (Xml.member "edns0clientsubnetmask" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_d_n_s0_client_subnet_mask
              (fun f ->
                 Query.Pair ("edns0clientsubnetmask", (String.to_query f)));
           Util.option_map v.e_d_n_s0_client_subnet_i_p
             (fun f ->
                Query.Pair ("edns0clientsubnetip", (String.to_query f)));
           Util.option_map v.resolver_i_p
             (fun f -> Query.Pair ("resolverip", (String.to_query f)));
           Some (Query.Pair ("recordtype", (RRType.to_query v.record_type)));
           Some (Query.Pair ("recordname", (String.to_query v.record_name)));
           Some
             (Query.Pair ("hostedzoneid", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_d_n_s0_client_subnet_mask
              (fun f -> ("e_d_n_s0_client_subnet_mask", (String.to_json f)));
           Util.option_map v.e_d_n_s0_client_subnet_i_p
             (fun f -> ("e_d_n_s0_client_subnet_i_p", (String.to_json f)));
           Util.option_map v.resolver_i_p
             (fun f -> ("resolver_i_p", (String.to_json f)));
           Some ("record_type", (RRType.to_json v.record_type));
           Some ("record_name", (String.to_json v.record_name));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        record_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "record_name")));
        record_type =
          (RRType.of_json (Util.of_option_exn (Json.lookup j "record_type")));
        resolver_i_p =
          (Util.option_map (Json.lookup j "resolver_i_p") String.of_json);
        e_d_n_s0_client_subnet_i_p =
          (Util.option_map (Json.lookup j "e_d_n_s0_client_subnet_i_p")
             String.of_json);
        e_d_n_s0_client_subnet_mask =
          (Util.option_map (Json.lookup j "e_d_n_s0_client_subnet_mask")
             String.of_json)
      }
  end
module NoSuchChange =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateQueryLoggingConfigRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      cloud_watch_logs_log_group_arn: String.t }
    let make ~hosted_zone_id  ~cloud_watch_logs_log_group_arn  () =
      { hosted_zone_id; cloud_watch_logs_log_group_arn }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          cloud_watch_logs_log_group_arn =
            (Xml.required "CloudWatchLogsLogGroupArn"
               (Util.option_bind (Xml.member "CloudWatchLogsLogGroupArn" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CloudWatchLogsLogGroupArn",
                   (String.to_query v.cloud_watch_logs_log_group_arn)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cloud_watch_logs_log_group_arn",
                (String.to_json v.cloud_watch_logs_log_group_arn));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        cloud_watch_logs_log_group_arn =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "cloud_watch_logs_log_group_arn")))
      }
  end
module GetReusableDelegationSetResponse =
  struct
    type t = {
      delegation_set: DelegationSet.t }
    let make ~delegation_set  () = { delegation_set }
    let parse xml =
      Some
        {
          delegation_set =
            (Xml.required "DelegationSet"
               (Util.option_bind (Xml.member "DelegationSet" xml)
                  DelegationSet.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DelegationSet", (DelegationSet.to_query v.delegation_set)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("delegation_set", (DelegationSet.to_json v.delegation_set))])
    let of_json j =
      {
        delegation_set =
          (DelegationSet.of_json
             (Util.of_option_exn (Json.lookup j "delegation_set")))
      }
  end
module ChangeResourceRecordSetsResponse =
  struct
    type t = {
      change_info: ChangeInfo.t }
    let make ~change_info  () = { change_info }
    let parse xml =
      Some
        {
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_info", (ChangeInfo.to_json v.change_info))])
    let of_json j =
      {
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")))
      }
  end
module ChangeTagsForResourceResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidChangeBatch =
  struct
    type t = {
      messages: ErrorMessages.t ;
      message: String.t option }
    let make ?(messages= [])  ?message  () = { messages; message }
    let parse xml =
      Some
        {
          messages =
            (Util.of_option []
               (Util.option_bind (Xml.member "messages" xml)
                  ErrorMessages.parse));
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)));
           Some
             (Query.Pair
                ("messages.member", (ErrorMessages.to_query v.messages)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Some ("messages", (ErrorMessages.to_json v.messages))])
    let of_json j =
      {
        messages =
          (ErrorMessages.of_json
             (Util.of_option_exn (Json.lookup j "messages")));
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTrafficPolicyInstancesByPolicyRequest =
  struct
    type t =
      {
      traffic_policy_id: String.t ;
      traffic_policy_version: Integer.t ;
      hosted_zone_id_marker: String.t option ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      max_items: String.t option }
    let make ~traffic_policy_id  ~traffic_policy_version 
      ?hosted_zone_id_marker  ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ?max_items  () =
      {
        traffic_policy_id;
        traffic_policy_version;
        hosted_zone_id_marker;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policy_id =
            (Xml.required "id"
               (Util.option_bind (Xml.member "id" xml) String.parse));
          traffic_policy_version =
            (Xml.required "version"
               (Util.option_bind (Xml.member "version" xml) Integer.parse));
          hosted_zone_id_marker =
            (Util.option_bind (Xml.member "hostedzoneid" xml) String.parse);
          traffic_policy_instance_name_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancename" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancetype" xml)
               RRType.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancetype", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancename", (String.to_query f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> Query.Pair ("hostedzoneid", (String.to_query f)));
           Some
             (Query.Pair
                ("version", (Integer.to_query v.traffic_policy_version)));
           Some (Query.Pair ("id", (String.to_query v.traffic_policy_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> ("hosted_zone_id_marker", (String.to_json f)));
           Some
             ("traffic_policy_version",
               (Integer.to_json v.traffic_policy_version));
           Some ("traffic_policy_id", (String.to_json v.traffic_policy_id))])
    let of_json j =
      {
        traffic_policy_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_id")));
        traffic_policy_version =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_version")));
        hosted_zone_id_marker =
          (Util.option_map (Json.lookup j "hosted_zone_id_marker")
             String.of_json);
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module QueryLoggingConfigAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetGeoLocationRequest =
  struct
    type t =
      {
      continent_code: String.t option ;
      country_code: String.t option ;
      subdivision_code: String.t option }
    let make ?continent_code  ?country_code  ?subdivision_code  () =
      { continent_code; country_code; subdivision_code }
    let parse xml =
      Some
        {
          continent_code =
            (Util.option_bind (Xml.member "continentcode" xml) String.parse);
          country_code =
            (Util.option_bind (Xml.member "countrycode" xml) String.parse);
          subdivision_code =
            (Util.option_bind (Xml.member "subdivisioncode" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subdivision_code
              (fun f -> Query.Pair ("subdivisioncode", (String.to_query f)));
           Util.option_map v.country_code
             (fun f -> Query.Pair ("countrycode", (String.to_query f)));
           Util.option_map v.continent_code
             (fun f -> Query.Pair ("continentcode", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subdivision_code
              (fun f -> ("subdivision_code", (String.to_json f)));
           Util.option_map v.country_code
             (fun f -> ("country_code", (String.to_json f)));
           Util.option_map v.continent_code
             (fun f -> ("continent_code", (String.to_json f)))])
    let of_json j =
      {
        continent_code =
          (Util.option_map (Json.lookup j "continent_code") String.of_json);
        country_code =
          (Util.option_map (Json.lookup j "country_code") String.of_json);
        subdivision_code =
          (Util.option_map (Json.lookup j "subdivision_code") String.of_json)
      }
  end
module NotAuthorizedException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTrafficPolicyVersionsRequest =
  struct
    type t =
      {
      id: String.t ;
      traffic_policy_version_marker: String.t option ;
      max_items: String.t option }
    let make ~id  ?traffic_policy_version_marker  ?max_items  () =
      { id; traffic_policy_version_marker; max_items }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          traffic_policy_version_marker =
            (Util.option_bind (Xml.member "trafficpolicyversion" xml)
               String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.traffic_policy_version_marker
             (fun f ->
                Query.Pair ("trafficpolicyversion", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.traffic_policy_version_marker
             (fun f -> ("traffic_policy_version_marker", (String.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        traffic_policy_version_marker =
          (Util.option_map (Json.lookup j "traffic_policy_version_marker")
             String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module GetHostedZoneCountRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TooManyVPCAssociationAuthorizations =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateHealthCheckRequest =
  struct
    type t =
      {
      health_check_id: String.t ;
      health_check_version: Long.t option ;
      i_p_address: String.t option ;
      port: Integer.t option ;
      resource_path: String.t option ;
      fully_qualified_domain_name: String.t option ;
      search_string: String.t option ;
      failure_threshold: Integer.t option ;
      inverted: Boolean.t option ;
      disabled: Boolean.t option ;
      health_threshold: Integer.t option ;
      child_health_checks: ChildHealthCheckList.t ;
      enable_s_n_i: Boolean.t option ;
      regions: HealthCheckRegionList.t ;
      alarm_identifier: AlarmIdentifier.t option ;
      insufficient_data_health_status: InsufficientDataHealthStatus.t option ;
      reset_elements: ResettableElementNameList.t }
    let make ~health_check_id  ?health_check_version  ?i_p_address  ?port 
      ?resource_path  ?fully_qualified_domain_name  ?search_string 
      ?failure_threshold  ?inverted  ?disabled  ?health_threshold 
      ?(child_health_checks= [])  ?enable_s_n_i  ?(regions= []) 
      ?alarm_identifier  ?insufficient_data_health_status  ?(reset_elements=
      [])  () =
      {
        health_check_id;
        health_check_version;
        i_p_address;
        port;
        resource_path;
        fully_qualified_domain_name;
        search_string;
        failure_threshold;
        inverted;
        disabled;
        health_threshold;
        child_health_checks;
        enable_s_n_i;
        regions;
        alarm_identifier;
        insufficient_data_health_status;
        reset_elements
      }
    let parse xml =
      Some
        {
          health_check_id =
            (Xml.required "HealthCheckId"
               (Util.option_bind (Xml.member "HealthCheckId" xml)
                  String.parse));
          health_check_version =
            (Util.option_bind (Xml.member "HealthCheckVersion" xml)
               Long.parse);
          i_p_address =
            (Util.option_bind (Xml.member "IPAddress" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          resource_path =
            (Util.option_bind (Xml.member "ResourcePath" xml) String.parse);
          fully_qualified_domain_name =
            (Util.option_bind (Xml.member "FullyQualifiedDomainName" xml)
               String.parse);
          search_string =
            (Util.option_bind (Xml.member "SearchString" xml) String.parse);
          failure_threshold =
            (Util.option_bind (Xml.member "FailureThreshold" xml)
               Integer.parse);
          inverted =
            (Util.option_bind (Xml.member "Inverted" xml) Boolean.parse);
          disabled =
            (Util.option_bind (Xml.member "Disabled" xml) Boolean.parse);
          health_threshold =
            (Util.option_bind (Xml.member "HealthThreshold" xml)
               Integer.parse);
          child_health_checks =
            (Util.of_option []
               (Util.option_bind (Xml.member "ChildHealthChecks" xml)
                  ChildHealthCheckList.parse));
          enable_s_n_i =
            (Util.option_bind (Xml.member "EnableSNI" xml) Boolean.parse);
          regions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Regions" xml)
                  HealthCheckRegionList.parse));
          alarm_identifier =
            (Util.option_bind (Xml.member "AlarmIdentifier" xml)
               AlarmIdentifier.parse);
          insufficient_data_health_status =
            (Util.option_bind (Xml.member "InsufficientDataHealthStatus" xml)
               InsufficientDataHealthStatus.parse);
          reset_elements =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResetElements" xml)
                  ResettableElementNameList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResetElements.member",
                   (ResettableElementNameList.to_query v.reset_elements)));
           Util.option_map v.insufficient_data_health_status
             (fun f ->
                Query.Pair
                  ("InsufficientDataHealthStatus",
                    (InsufficientDataHealthStatus.to_query f)));
           Util.option_map v.alarm_identifier
             (fun f ->
                Query.Pair ("AlarmIdentifier", (AlarmIdentifier.to_query f)));
           Some
             (Query.Pair
                ("Regions.member",
                  (HealthCheckRegionList.to_query v.regions)));
           Util.option_map v.enable_s_n_i
             (fun f -> Query.Pair ("EnableSNI", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("ChildHealthChecks.member",
                  (ChildHealthCheckList.to_query v.child_health_checks)));
           Util.option_map v.health_threshold
             (fun f -> Query.Pair ("HealthThreshold", (Integer.to_query f)));
           Util.option_map v.disabled
             (fun f -> Query.Pair ("Disabled", (Boolean.to_query f)));
           Util.option_map v.inverted
             (fun f -> Query.Pair ("Inverted", (Boolean.to_query f)));
           Util.option_map v.failure_threshold
             (fun f -> Query.Pair ("FailureThreshold", (Integer.to_query f)));
           Util.option_map v.search_string
             (fun f -> Query.Pair ("SearchString", (String.to_query f)));
           Util.option_map v.fully_qualified_domain_name
             (fun f ->
                Query.Pair ("FullyQualifiedDomainName", (String.to_query f)));
           Util.option_map v.resource_path
             (fun f -> Query.Pair ("ResourcePath", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.i_p_address
             (fun f -> Query.Pair ("IPAddress", (String.to_query f)));
           Util.option_map v.health_check_version
             (fun f -> Query.Pair ("HealthCheckVersion", (Long.to_query f)));
           Some
             (Query.Pair
                ("HealthCheckId", (String.to_query v.health_check_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reset_elements",
                (ResettableElementNameList.to_json v.reset_elements));
           Util.option_map v.insufficient_data_health_status
             (fun f ->
                ("insufficient_data_health_status",
                  (InsufficientDataHealthStatus.to_json f)));
           Util.option_map v.alarm_identifier
             (fun f -> ("alarm_identifier", (AlarmIdentifier.to_json f)));
           Some ("regions", (HealthCheckRegionList.to_json v.regions));
           Util.option_map v.enable_s_n_i
             (fun f -> ("enable_s_n_i", (Boolean.to_json f)));
           Some
             ("child_health_checks",
               (ChildHealthCheckList.to_json v.child_health_checks));
           Util.option_map v.health_threshold
             (fun f -> ("health_threshold", (Integer.to_json f)));
           Util.option_map v.disabled
             (fun f -> ("disabled", (Boolean.to_json f)));
           Util.option_map v.inverted
             (fun f -> ("inverted", (Boolean.to_json f)));
           Util.option_map v.failure_threshold
             (fun f -> ("failure_threshold", (Integer.to_json f)));
           Util.option_map v.search_string
             (fun f -> ("search_string", (String.to_json f)));
           Util.option_map v.fully_qualified_domain_name
             (fun f -> ("fully_qualified_domain_name", (String.to_json f)));
           Util.option_map v.resource_path
             (fun f -> ("resource_path", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.i_p_address
             (fun f -> ("i_p_address", (String.to_json f)));
           Util.option_map v.health_check_version
             (fun f -> ("health_check_version", (Long.to_json f)));
           Some ("health_check_id", (String.to_json v.health_check_id))])
    let of_json j =
      {
        health_check_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "health_check_id")));
        health_check_version =
          (Util.option_map (Json.lookup j "health_check_version")
             Long.of_json);
        i_p_address =
          (Util.option_map (Json.lookup j "i_p_address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        resource_path =
          (Util.option_map (Json.lookup j "resource_path") String.of_json);
        fully_qualified_domain_name =
          (Util.option_map (Json.lookup j "fully_qualified_domain_name")
             String.of_json);
        search_string =
          (Util.option_map (Json.lookup j "search_string") String.of_json);
        failure_threshold =
          (Util.option_map (Json.lookup j "failure_threshold")
             Integer.of_json);
        inverted =
          (Util.option_map (Json.lookup j "inverted") Boolean.of_json);
        disabled =
          (Util.option_map (Json.lookup j "disabled") Boolean.of_json);
        health_threshold =
          (Util.option_map (Json.lookup j "health_threshold") Integer.of_json);
        child_health_checks =
          (ChildHealthCheckList.of_json
             (Util.of_option_exn (Json.lookup j "child_health_checks")));
        enable_s_n_i =
          (Util.option_map (Json.lookup j "enable_s_n_i") Boolean.of_json);
        regions =
          (HealthCheckRegionList.of_json
             (Util.of_option_exn (Json.lookup j "regions")));
        alarm_identifier =
          (Util.option_map (Json.lookup j "alarm_identifier")
             AlarmIdentifier.of_json);
        insufficient_data_health_status =
          (Util.option_map (Json.lookup j "insufficient_data_health_status")
             InsufficientDataHealthStatus.of_json);
        reset_elements =
          (ResettableElementNameList.of_json
             (Util.of_option_exn (Json.lookup j "reset_elements")))
      }
  end
module GetHealthCheckCountResponse =
  struct
    type t = {
      health_check_count: Long.t }
    let make ~health_check_count  () = { health_check_count }
    let parse xml =
      Some
        {
          health_check_count =
            (Xml.required "HealthCheckCount"
               (Util.option_bind (Xml.member "HealthCheckCount" xml)
                  Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckCount", (Long.to_query v.health_check_count)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check_count", (Long.to_json v.health_check_count))])
    let of_json j =
      {
        health_check_count =
          (Long.of_json
             (Util.of_option_exn (Json.lookup j "health_check_count")))
      }
  end
module ListVPCAssociationAuthorizationsResponse =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      next_token: String.t option ;
      v_p_cs: VPCs.t }
    let make ~hosted_zone_id  ?next_token  ~v_p_cs  () =
      { hosted_zone_id; next_token; v_p_cs }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          v_p_cs =
            (Xml.required "VPCs"
               (Util.option_bind (Xml.member "VPCs" xml) VPCs.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("VPCs.member", (VPCs.to_query v.v_p_cs)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("v_p_cs", (VPCs.to_json v.v_p_cs));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        v_p_cs = (VPCs.of_json (Util.of_option_exn (Json.lookup j "v_p_cs")))
      }
  end
module CreateVPCAssociationAuthorizationResponse =
  struct
    type t = {
      hosted_zone_id: String.t ;
      v_p_c: VPC.t }
    let make ~hosted_zone_id  ~v_p_c  () = { hosted_zone_id; v_p_c }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "HostedZoneId"
               (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse));
          v_p_c =
            (Xml.required "VPC"
               (Util.option_bind (Xml.member "VPC" xml) VPC.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("VPC", (VPC.to_query v.v_p_c)));
           Some
             (Query.Pair ("HostedZoneId", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("v_p_c", (VPC.to_json v.v_p_c));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        v_p_c = (VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c")))
      }
  end
module GetQueryLoggingConfigRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module NoSuchHostedZone =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TrafficPolicyAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteReusableDelegationSetRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module InvalidTrafficPolicyDocument =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module VPCAssociationNotFound =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ConflictingDomainExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module NoSuchCloudWatchLogsLogGroup =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTagsForResourcesRequest =
  struct
    type t =
      {
      resource_type: TagResourceType.t ;
      resource_ids: TagResourceIdList.t }
    let make ~resource_type  ~resource_ids  () =
      { resource_type; resource_ids }
    let parse xml =
      Some
        {
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml)
                  TagResourceType.parse));
          resource_ids =
            (Xml.required "ResourceIds"
               (Util.option_bind (Xml.member "ResourceIds" xml)
                  TagResourceIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceIds.member",
                   (TagResourceIdList.to_query v.resource_ids)));
           Some
             (Query.Pair
                ("ResourceType", (TagResourceType.to_query v.resource_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("resource_ids", (TagResourceIdList.to_json v.resource_ids));
           Some ("resource_type", (TagResourceType.to_json v.resource_type))])
    let of_json j =
      {
        resource_type =
          (TagResourceType.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        resource_ids =
          (TagResourceIdList.of_json
             (Util.of_option_exn (Json.lookup j "resource_ids")))
      }
  end
module ListHostedZonesRequest =
  struct
    type t =
      {
      marker: String.t option ;
      max_items: String.t option ;
      delegation_set_id: String.t option }
    let make ?marker  ?max_items  ?delegation_set_id  () =
      { marker; max_items; delegation_set_id }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "marker" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse);
          delegation_set_id =
            (Util.option_bind (Xml.member "delegationsetid" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.delegation_set_id
              (fun f -> Query.Pair ("delegationsetid", (String.to_query f)));
           Util.option_map v.max_items
             (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.delegation_set_id
              (fun f -> ("delegation_set_id", (String.to_json f)));
           Util.option_map v.max_items
             (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json);
        delegation_set_id =
          (Util.option_map (Json.lookup j "delegation_set_id") String.of_json)
      }
  end
module ListHostedZonesResponse =
  struct
    type t =
      {
      hosted_zones: HostedZones.t ;
      marker: String.t option ;
      is_truncated: Boolean.t ;
      next_marker: String.t option ;
      max_items: String.t }
    let make ~hosted_zones  ?marker  ~is_truncated  ?next_marker  ~max_items 
      () = { hosted_zones; marker; is_truncated; next_marker; max_items }
    let parse xml =
      Some
        {
          hosted_zones =
            (Xml.required "HostedZones"
               (Util.option_bind (Xml.member "HostedZones" xml)
                  HostedZones.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("HostedZones.member", (HostedZones.to_query v.hosted_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some ("hosted_zones", (HostedZones.to_json v.hosted_zones))])
    let of_json j =
      {
        hosted_zones =
          (HostedZones.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zones")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module CreateReusableDelegationSetRequest =
  struct
    type t = {
      caller_reference: String.t ;
      hosted_zone_id: String.t option }
    let make ~caller_reference  ?hosted_zone_id  () =
      { caller_reference; hosted_zone_id }
    let parse xml =
      Some
        {
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          hosted_zone_id =
            (Util.option_bind (Xml.member "HostedZoneId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.hosted_zone_id
              (fun f -> Query.Pair ("HostedZoneId", (String.to_query f)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.hosted_zone_id
              (fun f -> ("hosted_zone_id", (String.to_json f)));
           Some ("caller_reference", (String.to_json v.caller_reference))])
    let of_json j =
      {
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        hosted_zone_id =
          (Util.option_map (Json.lookup j "hosted_zone_id") String.of_json)
      }
  end
module ListTagsForResourceResponse =
  struct
    type t = {
      resource_tag_set: ResourceTagSet.t }
    let make ~resource_tag_set  () = { resource_tag_set }
    let parse xml =
      Some
        {
          resource_tag_set =
            (Xml.required "ResourceTagSet"
               (Util.option_bind (Xml.member "ResourceTagSet" xml)
                  ResourceTagSet.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceTagSet",
                   (ResourceTagSet.to_query v.resource_tag_set)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_tag_set",
                (ResourceTagSet.to_json v.resource_tag_set))])
    let of_json j =
      {
        resource_tag_set =
          (ResourceTagSet.of_json
             (Util.of_option_exn (Json.lookup j "resource_tag_set")))
      }
  end
module CreateHealthCheckResponse =
  struct
    type t = {
      health_check: HealthCheck.t ;
      location: String.t }
    let make ~health_check  ~location  () = { health_check; location }
    let parse xml =
      Some
        {
          health_check =
            (Xml.required "HealthCheck"
               (Util.option_bind (Xml.member "HealthCheck" xml)
                  HealthCheck.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("HealthCheck", (HealthCheck.to_query v.health_check)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some ("health_check", (HealthCheck.to_json v.health_check))])
    let of_json j =
      {
        health_check =
          (HealthCheck.of_json
             (Util.of_option_exn (Json.lookup j "health_check")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module GetChangeResponse =
  struct
    type t = {
      change_info: ChangeInfo.t }
    let make ~change_info  () = { change_info }
    let parse xml =
      Some
        {
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_info", (ChangeInfo.to_json v.change_info))])
    let of_json j =
      {
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")))
      }
  end
module InvalidVPCId =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListGeoLocationsResponse =
  struct
    type t =
      {
      geo_location_details_list: GeoLocationDetailsList.t ;
      is_truncated: Boolean.t ;
      next_continent_code: String.t option ;
      next_country_code: String.t option ;
      next_subdivision_code: String.t option ;
      max_items: String.t }
    let make ~geo_location_details_list  ~is_truncated  ?next_continent_code 
      ?next_country_code  ?next_subdivision_code  ~max_items  () =
      {
        geo_location_details_list;
        is_truncated;
        next_continent_code;
        next_country_code;
        next_subdivision_code;
        max_items
      }
    let parse xml =
      Some
        {
          geo_location_details_list =
            (Xml.required "GeoLocationDetailsList"
               (Util.option_bind (Xml.member "GeoLocationDetailsList" xml)
                  GeoLocationDetailsList.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_continent_code =
            (Util.option_bind (Xml.member "NextContinentCode" xml)
               String.parse);
          next_country_code =
            (Util.option_bind (Xml.member "NextCountryCode" xml) String.parse);
          next_subdivision_code =
            (Util.option_bind (Xml.member "NextSubdivisionCode" xml)
               String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_subdivision_code
             (fun f ->
                Query.Pair ("NextSubdivisionCode", (String.to_query f)));
           Util.option_map v.next_country_code
             (fun f -> Query.Pair ("NextCountryCode", (String.to_query f)));
           Util.option_map v.next_continent_code
             (fun f -> Query.Pair ("NextContinentCode", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some
             (Query.Pair
                ("GeoLocationDetailsList.member",
                  (GeoLocationDetailsList.to_query
                     v.geo_location_details_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_subdivision_code
             (fun f -> ("next_subdivision_code", (String.to_json f)));
           Util.option_map v.next_country_code
             (fun f -> ("next_country_code", (String.to_json f)));
           Util.option_map v.next_continent_code
             (fun f -> ("next_continent_code", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some
             ("geo_location_details_list",
               (GeoLocationDetailsList.to_json v.geo_location_details_list))])
    let of_json j =
      {
        geo_location_details_list =
          (GeoLocationDetailsList.of_json
             (Util.of_option_exn (Json.lookup j "geo_location_details_list")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_continent_code =
          (Util.option_map (Json.lookup j "next_continent_code")
             String.of_json);
        next_country_code =
          (Util.option_map (Json.lookup j "next_country_code") String.of_json);
        next_subdivision_code =
          (Util.option_map (Json.lookup j "next_subdivision_code")
             String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module UpdateTrafficPolicyInstanceRequest =
  struct
    type t =
      {
      id: String.t ;
      t_t_l: Long.t ;
      traffic_policy_id: String.t ;
      traffic_policy_version: Integer.t }
    let make ~id  ~t_t_l  ~traffic_policy_id  ~traffic_policy_version  () =
      { id; t_t_l; traffic_policy_id; traffic_policy_version }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          t_t_l =
            (Xml.required "TTL"
               (Util.option_bind (Xml.member "TTL" xml) Long.parse));
          traffic_policy_id =
            (Xml.required "TrafficPolicyId"
               (Util.option_bind (Xml.member "TrafficPolicyId" xml)
                  String.parse));
          traffic_policy_version =
            (Xml.required "TrafficPolicyVersion"
               (Util.option_bind (Xml.member "TrafficPolicyVersion" xml)
                  Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyVersion",
                   (Integer.to_query v.traffic_policy_version)));
           Some
             (Query.Pair
                ("TrafficPolicyId", (String.to_query v.traffic_policy_id)));
           Some (Query.Pair ("TTL", (Long.to_query v.t_t_l)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_version",
                (Integer.to_json v.traffic_policy_version));
           Some ("traffic_policy_id", (String.to_json v.traffic_policy_id));
           Some ("t_t_l", (Long.to_json v.t_t_l));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        t_t_l = (Long.of_json (Util.of_option_exn (Json.lookup j "t_t_l")));
        traffic_policy_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_id")));
        traffic_policy_version =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_version")))
      }
  end
module GetHostedZoneLimitResponse =
  struct
    type t = {
      limit: HostedZoneLimit.t ;
      count: Long.t }
    let make ~limit  ~count  () = { limit; count }
    let parse xml =
      Some
        {
          limit =
            (Xml.required "Limit"
               (Util.option_bind (Xml.member "Limit" xml)
                  HostedZoneLimit.parse));
          count =
            (Xml.required "Count"
               (Util.option_bind (Xml.member "Count" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Count", (Long.to_query v.count)));
           Some (Query.Pair ("Limit", (HostedZoneLimit.to_query v.limit)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("count", (Long.to_json v.count));
           Some ("limit", (HostedZoneLimit.to_json v.limit))])
    let of_json j =
      {
        limit =
          (HostedZoneLimit.of_json
             (Util.of_option_exn (Json.lookup j "limit")));
        count = (Long.of_json (Util.of_option_exn (Json.lookup j "count")))
      }
  end
module ListHealthChecksResponse =
  struct
    type t =
      {
      health_checks: HealthChecks.t ;
      marker: String.t ;
      is_truncated: Boolean.t ;
      next_marker: String.t option ;
      max_items: String.t }
    let make ~health_checks  ~marker  ~is_truncated  ?next_marker  ~max_items
       () = { health_checks; marker; is_truncated; next_marker; max_items }
    let parse xml =
      Some
        {
          health_checks =
            (Xml.required "HealthChecks"
               (Util.option_bind (Xml.member "HealthChecks" xml)
                  HealthChecks.parse));
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)));
           Some
             (Query.Pair
                ("HealthChecks.member",
                  (HealthChecks.to_query v.health_checks)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("marker", (String.to_json v.marker));
           Some ("health_checks", (HealthChecks.to_json v.health_checks))])
    let of_json j =
      {
        health_checks =
          (HealthChecks.of_json
             (Util.of_option_exn (Json.lookup j "health_checks")));
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module IncompatibleVersion =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListQueryLoggingConfigsResponse =
  struct
    type t =
      {
      query_logging_configs: QueryLoggingConfigs.t ;
      next_token: String.t option }
    let make ~query_logging_configs  ?next_token  () =
      { query_logging_configs; next_token }
    let parse xml =
      Some
        {
          query_logging_configs =
            (Xml.required "QueryLoggingConfigs"
               (Util.option_bind (Xml.member "QueryLoggingConfigs" xml)
                  QueryLoggingConfigs.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("QueryLoggingConfigs.member",
                  (QueryLoggingConfigs.to_query v.query_logging_configs)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("query_logging_configs",
               (QueryLoggingConfigs.to_json v.query_logging_configs))])
    let of_json j =
      {
        query_logging_configs =
          (QueryLoggingConfigs.of_json
             (Util.of_option_exn (Json.lookup j "query_logging_configs")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateTrafficPolicyInstanceResponse =
  struct
    type t =
      {
      traffic_policy_instance: TrafficPolicyInstance.t ;
      location: String.t }
    let make ~traffic_policy_instance  ~location  () =
      { traffic_policy_instance; location }
    let parse xml =
      Some
        {
          traffic_policy_instance =
            (Xml.required "TrafficPolicyInstance"
               (Util.option_bind (Xml.member "TrafficPolicyInstance" xml)
                  TrafficPolicyInstance.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("TrafficPolicyInstance",
                  (TrafficPolicyInstance.to_query v.traffic_policy_instance)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some
             ("traffic_policy_instance",
               (TrafficPolicyInstance.to_json v.traffic_policy_instance))])
    let of_json j =
      {
        traffic_policy_instance =
          (TrafficPolicyInstance.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instance")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module HealthCheckInUse =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TooManyHealthChecks =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetTrafficPolicyInstanceCountResponse =
  struct
    type t = {
      traffic_policy_instance_count: Integer.t }
    let make ~traffic_policy_instance_count  () =
      { traffic_policy_instance_count }
    let parse xml =
      Some
        {
          traffic_policy_instance_count =
            (Xml.required "TrafficPolicyInstanceCount"
               (Util.option_bind
                  (Xml.member "TrafficPolicyInstanceCount" xml) Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyInstanceCount",
                   (Integer.to_query v.traffic_policy_instance_count)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_instance_count",
                (Integer.to_json v.traffic_policy_instance_count))])
    let of_json j =
      {
        traffic_policy_instance_count =
          (Integer.of_json
             (Util.of_option_exn
                (Json.lookup j "traffic_policy_instance_count")))
      }
  end
module DeleteHostedZoneRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module UpdateHealthCheckResponse =
  struct
    type t = {
      health_check: HealthCheck.t }
    let make ~health_check  () = { health_check }
    let parse xml =
      Some
        {
          health_check =
            (Xml.required "HealthCheck"
               (Util.option_bind (Xml.member "HealthCheck" xml)
                  HealthCheck.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheck", (HealthCheck.to_query v.health_check)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check", (HealthCheck.to_json v.health_check))])
    let of_json j =
      {
        health_check =
          (HealthCheck.of_json
             (Util.of_option_exn (Json.lookup j "health_check")))
      }
  end
module ChangeTagsForResourceRequest =
  struct
    type t =
      {
      resource_type: TagResourceType.t ;
      resource_id: String.t ;
      add_tags: TagList.t ;
      remove_tag_keys: TagKeyList.t }
    let make ~resource_type  ~resource_id  ?(add_tags= []) 
      ?(remove_tag_keys= [])  () =
      { resource_type; resource_id; add_tags; remove_tag_keys }
    let parse xml =
      Some
        {
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml)
                  TagResourceType.parse));
          resource_id =
            (Xml.required "ResourceId"
               (Util.option_bind (Xml.member "ResourceId" xml) String.parse));
          add_tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "AddTags" xml) TagList.parse));
          remove_tag_keys =
            (Util.of_option []
               (Util.option_bind (Xml.member "RemoveTagKeys" xml)
                  TagKeyList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RemoveTagKeys.member",
                   (TagKeyList.to_query v.remove_tag_keys)));
           Some
             (Query.Pair ("AddTags.member", (TagList.to_query v.add_tags)));
           Some (Query.Pair ("ResourceId", (String.to_query v.resource_id)));
           Some
             (Query.Pair
                ("ResourceType", (TagResourceType.to_query v.resource_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("remove_tag_keys", (TagKeyList.to_json v.remove_tag_keys));
           Some ("add_tags", (TagList.to_json v.add_tags));
           Some ("resource_id", (String.to_json v.resource_id));
           Some ("resource_type", (TagResourceType.to_json v.resource_type))])
    let of_json j =
      {
        resource_type =
          (TagResourceType.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        resource_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "resource_id")));
        add_tags =
          (TagList.of_json (Util.of_option_exn (Json.lookup j "add_tags")));
        remove_tag_keys =
          (TagKeyList.of_json
             (Util.of_option_exn (Json.lookup j "remove_tag_keys")))
      }
  end
module CreateQueryLoggingConfigResponse =
  struct
    type t =
      {
      query_logging_config: QueryLoggingConfig.t ;
      location: String.t }
    let make ~query_logging_config  ~location  () =
      { query_logging_config; location }
    let parse xml =
      Some
        {
          query_logging_config =
            (Xml.required "QueryLoggingConfig"
               (Util.option_bind (Xml.member "QueryLoggingConfig" xml)
                  QueryLoggingConfig.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("QueryLoggingConfig",
                  (QueryLoggingConfig.to_query v.query_logging_config)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some
             ("query_logging_config",
               (QueryLoggingConfig.to_json v.query_logging_config))])
    let of_json j =
      {
        query_logging_config =
          (QueryLoggingConfig.of_json
             (Util.of_option_exn (Json.lookup j "query_logging_config")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module TooManyTrafficPolicies =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListQueryLoggingConfigsRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t option ;
      next_token: String.t option ;
      max_results: String.t option }
    let make ?hosted_zone_id  ?next_token  ?max_results  () =
      { hosted_zone_id; next_token; max_results }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Util.option_bind (Xml.member "hostedzoneid" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "nexttoken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "maxresults" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("maxresults", (String.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("nexttoken", (String.to_query f)));
           Util.option_map v.hosted_zone_id
             (fun f -> Query.Pair ("hostedzoneid", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (String.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.hosted_zone_id
             (fun f -> ("hosted_zone_id", (String.to_json f)))])
    let of_json j =
      {
        hosted_zone_id =
          (Util.option_map (Json.lookup j "hosted_zone_id") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") String.of_json)
      }
  end
module DelegationSetNotAvailable =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteQueryLoggingConfigRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module GetReusableDelegationSetLimitRequest =
  struct
    type t =
      {
      type_: ReusableDelegationSetLimitType.t ;
      delegation_set_id: String.t }
    let make ~type_  ~delegation_set_id  () = { type_; delegation_set_id }
    let parse xml =
      Some
        {
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml)
                  ReusableDelegationSetLimitType.parse));
          delegation_set_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.delegation_set_id)));
           Some
             (Query.Pair
                ("Type", (ReusableDelegationSetLimitType.to_query v.type_)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("delegation_set_id", (String.to_json v.delegation_set_id));
           Some ("type_", (ReusableDelegationSetLimitType.to_json v.type_))])
    let of_json j =
      {
        type_ =
          (ReusableDelegationSetLimitType.of_json
             (Util.of_option_exn (Json.lookup j "type_")));
        delegation_set_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "delegation_set_id")))
      }
  end
module ListHostedZonesByVPCRequest =
  struct
    type t =
      {
      v_p_c_id: String.t ;
      v_p_c_region: VPCRegion.t ;
      max_items: String.t option ;
      next_token: String.t option }
    let make ~v_p_c_id  ~v_p_c_region  ?max_items  ?next_token  () =
      { v_p_c_id; v_p_c_region; max_items; next_token }
    let parse xml =
      Some
        {
          v_p_c_id =
            (Xml.required "vpcid"
               (Util.option_bind (Xml.member "vpcid" xml) String.parse));
          v_p_c_region =
            (Xml.required "vpcregion"
               (Util.option_bind (Xml.member "vpcregion" xml) VPCRegion.parse));
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "nexttoken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("nexttoken", (String.to_query f)));
           Util.option_map v.max_items
             (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Some
             (Query.Pair ("vpcregion", (VPCRegion.to_query v.v_p_c_region)));
           Some (Query.Pair ("vpcid", (String.to_query v.v_p_c_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_items
             (fun f -> ("max_items", (String.to_json f)));
           Some ("v_p_c_region", (VPCRegion.to_json v.v_p_c_region));
           Some ("v_p_c_id", (String.to_json v.v_p_c_id))])
    let of_json j =
      {
        v_p_c_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "v_p_c_id")));
        v_p_c_region =
          (VPCRegion.of_json
             (Util.of_option_exn (Json.lookup j "v_p_c_region")));
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTagsForResourcesResponse =
  struct
    type t = {
      resource_tag_sets: ResourceTagSetList.t }
    let make ~resource_tag_sets  () = { resource_tag_sets }
    let parse xml =
      Some
        {
          resource_tag_sets =
            (Xml.required "ResourceTagSets"
               (Util.option_bind (Xml.member "ResourceTagSets" xml)
                  ResourceTagSetList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceTagSets.member",
                   (ResourceTagSetList.to_query v.resource_tag_sets)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_tag_sets",
                (ResourceTagSetList.to_json v.resource_tag_sets))])
    let of_json j =
      {
        resource_tag_sets =
          (ResourceTagSetList.of_json
             (Util.of_option_exn (Json.lookup j "resource_tag_sets")))
      }
  end
module AssociateVPCWithHostedZoneRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      v_p_c: VPC.t ;
      comment: String.t option }
    let make ~hosted_zone_id  ~v_p_c  ?comment  () =
      { hosted_zone_id; v_p_c; comment }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          v_p_c =
            (Xml.required "VPC"
               (Util.option_bind (Xml.member "VPC" xml) VPC.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("VPC", (VPC.to_query v.v_p_c)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("v_p_c", (VPC.to_json v.v_p_c));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        v_p_c = (VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module GetHostedZoneCountResponse =
  struct
    type t = {
      hosted_zone_count: Long.t }
    let make ~hosted_zone_count  () = { hosted_zone_count }
    let parse xml =
      Some
        {
          hosted_zone_count =
            (Xml.required "HostedZoneCount"
               (Util.option_bind (Xml.member "HostedZoneCount" xml)
                  Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HostedZoneCount", (Long.to_query v.hosted_zone_count)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("hosted_zone_count", (Long.to_json v.hosted_zone_count))])
    let of_json j =
      {
        hosted_zone_count =
          (Long.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_count")))
      }
  end
module TooManyHostedZones =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DisassociateVPCFromHostedZoneRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      v_p_c: VPC.t ;
      comment: String.t option }
    let make ~hosted_zone_id  ~v_p_c  ?comment  () =
      { hosted_zone_id; v_p_c; comment }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          v_p_c =
            (Xml.required "VPC"
               (Util.option_bind (Xml.member "VPC" xml) VPC.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("VPC", (VPC.to_query v.v_p_c)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("v_p_c", (VPC.to_json v.v_p_c));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        v_p_c = (VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module GetTrafficPolicyInstanceCountRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListGeoLocationsRequest =
  struct
    type t =
      {
      start_continent_code: String.t option ;
      start_country_code: String.t option ;
      start_subdivision_code: String.t option ;
      max_items: String.t option }
    let make ?start_continent_code  ?start_country_code 
      ?start_subdivision_code  ?max_items  () =
      {
        start_continent_code;
        start_country_code;
        start_subdivision_code;
        max_items
      }
    let parse xml =
      Some
        {
          start_continent_code =
            (Util.option_bind (Xml.member "startcontinentcode" xml)
               String.parse);
          start_country_code =
            (Util.option_bind (Xml.member "startcountrycode" xml)
               String.parse);
          start_subdivision_code =
            (Util.option_bind (Xml.member "startsubdivisioncode" xml)
               String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.start_subdivision_code
             (fun f ->
                Query.Pair ("startsubdivisioncode", (String.to_query f)));
           Util.option_map v.start_country_code
             (fun f -> Query.Pair ("startcountrycode", (String.to_query f)));
           Util.option_map v.start_continent_code
             (fun f -> Query.Pair ("startcontinentcode", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.start_subdivision_code
             (fun f -> ("start_subdivision_code", (String.to_json f)));
           Util.option_map v.start_country_code
             (fun f -> ("start_country_code", (String.to_json f)));
           Util.option_map v.start_continent_code
             (fun f -> ("start_continent_code", (String.to_json f)))])
    let of_json j =
      {
        start_continent_code =
          (Util.option_map (Json.lookup j "start_continent_code")
             String.of_json);
        start_country_code =
          (Util.option_map (Json.lookup j "start_country_code")
             String.of_json);
        start_subdivision_code =
          (Util.option_map (Json.lookup j "start_subdivision_code")
             String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module ListTrafficPolicyInstancesByHostedZoneRequest =
  struct
    type t =
      {
      hosted_zone_id: String.t ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      max_items: String.t option }
    let make ~hosted_zone_id  ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ?max_items  () =
      {
        hosted_zone_id;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        max_items
      }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "id"
               (Util.option_bind (Xml.member "id" xml) String.parse));
          traffic_policy_instance_name_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancename" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancetype" xml)
               RRType.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancetype", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancename", (String.to_query f)));
           Some (Query.Pair ("id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module CreateReusableDelegationSetResponse =
  struct
    type t = {
      delegation_set: DelegationSet.t ;
      location: String.t }
    let make ~delegation_set  ~location  () = { delegation_set; location }
    let parse xml =
      Some
        {
          delegation_set =
            (Xml.required "DelegationSet"
               (Util.option_bind (Xml.member "DelegationSet" xml)
                  DelegationSet.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("DelegationSet", (DelegationSet.to_query v.delegation_set)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some ("delegation_set", (DelegationSet.to_json v.delegation_set))])
    let of_json j =
      {
        delegation_set =
          (DelegationSet.of_json
             (Util.of_option_exn (Json.lookup j "delegation_set")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module GetAccountLimitResponse =
  struct
    type t = {
      limit: AccountLimit.t ;
      count: Long.t }
    let make ~limit  ~count  () = { limit; count }
    let parse xml =
      Some
        {
          limit =
            (Xml.required "Limit"
               (Util.option_bind (Xml.member "Limit" xml) AccountLimit.parse));
          count =
            (Xml.required "Count"
               (Util.option_bind (Xml.member "Count" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Count", (Long.to_query v.count)));
           Some (Query.Pair ("Limit", (AccountLimit.to_query v.limit)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("count", (Long.to_json v.count));
           Some ("limit", (AccountLimit.to_json v.limit))])
    let of_json j =
      {
        limit =
          (AccountLimit.of_json (Util.of_option_exn (Json.lookup j "limit")));
        count = (Long.of_json (Util.of_option_exn (Json.lookup j "count")))
      }
  end
module DisassociateVPCFromHostedZoneResponse =
  struct
    type t = {
      change_info: ChangeInfo.t }
    let make ~change_info  () = { change_info }
    let parse xml =
      Some
        {
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_info", (ChangeInfo.to_json v.change_info))])
    let of_json j =
      {
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")))
      }
  end
module ListResourceRecordSetsResponse =
  struct
    type t =
      {
      resource_record_sets: ResourceRecordSets.t ;
      is_truncated: Boolean.t ;
      next_record_name: String.t option ;
      next_record_type: RRType.t option ;
      next_record_identifier: String.t option ;
      max_items: String.t }
    let make ~resource_record_sets  ~is_truncated  ?next_record_name 
      ?next_record_type  ?next_record_identifier  ~max_items  () =
      {
        resource_record_sets;
        is_truncated;
        next_record_name;
        next_record_type;
        next_record_identifier;
        max_items
      }
    let parse xml =
      Some
        {
          resource_record_sets =
            (Xml.required "ResourceRecordSets"
               (Util.option_bind (Xml.member "ResourceRecordSets" xml)
                  ResourceRecordSets.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_record_name =
            (Util.option_bind (Xml.member "NextRecordName" xml) String.parse);
          next_record_type =
            (Util.option_bind (Xml.member "NextRecordType" xml) RRType.parse);
          next_record_identifier =
            (Util.option_bind (Xml.member "NextRecordIdentifier" xml)
               String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_record_identifier
             (fun f ->
                Query.Pair ("NextRecordIdentifier", (String.to_query f)));
           Util.option_map v.next_record_type
             (fun f -> Query.Pair ("NextRecordType", (RRType.to_query f)));
           Util.option_map v.next_record_name
             (fun f -> Query.Pair ("NextRecordName", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some
             (Query.Pair
                ("ResourceRecordSets.member",
                  (ResourceRecordSets.to_query v.resource_record_sets)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_record_identifier
             (fun f -> ("next_record_identifier", (String.to_json f)));
           Util.option_map v.next_record_type
             (fun f -> ("next_record_type", (RRType.to_json f)));
           Util.option_map v.next_record_name
             (fun f -> ("next_record_name", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some
             ("resource_record_sets",
               (ResourceRecordSets.to_json v.resource_record_sets))])
    let of_json j =
      {
        resource_record_sets =
          (ResourceRecordSets.of_json
             (Util.of_option_exn (Json.lookup j "resource_record_sets")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_record_name =
          (Util.option_map (Json.lookup j "next_record_name") String.of_json);
        next_record_type =
          (Util.option_map (Json.lookup j "next_record_type") RRType.of_json);
        next_record_identifier =
          (Util.option_map (Json.lookup j "next_record_identifier")
             String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module ListTrafficPoliciesRequest =
  struct
    type t =
      {
      traffic_policy_id_marker: String.t option ;
      max_items: String.t option }
    let make ?traffic_policy_id_marker  ?max_items  () =
      { traffic_policy_id_marker; max_items }
    let parse xml =
      Some
        {
          traffic_policy_id_marker =
            (Util.option_bind (Xml.member "trafficpolicyid" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.traffic_policy_id_marker
             (fun f -> Query.Pair ("trafficpolicyid", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.traffic_policy_id_marker
             (fun f -> ("traffic_policy_id_marker", (String.to_json f)))])
    let of_json j =
      {
        traffic_policy_id_marker =
          (Util.option_map (Json.lookup j "traffic_policy_id_marker")
             String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module NoSuchGeoLocation =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateTrafficPolicyCommentRequest =
  struct
    type t = {
      id: String.t ;
      version: Integer.t ;
      comment: String.t }
    let make ~id  ~version  ~comment  () = { id; version; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          version =
            (Xml.required "Version"
               (Util.option_bind (Xml.member "Version" xml) Integer.parse));
          comment =
            (Xml.required "Comment"
               (Util.option_bind (Xml.member "Comment" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Comment", (String.to_query v.comment)));
           Some (Query.Pair ("Version", (Integer.to_query v.version)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("comment", (String.to_json v.comment));
           Some ("version", (Integer.to_json v.version));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        version =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "version")));
        comment =
          (String.of_json (Util.of_option_exn (Json.lookup j "comment")))
      }
  end
module UpdateTrafficPolicyInstanceResponse =
  struct
    type t = {
      traffic_policy_instance: TrafficPolicyInstance.t }
    let make ~traffic_policy_instance  () = { traffic_policy_instance }
    let parse xml =
      Some
        {
          traffic_policy_instance =
            (Xml.required "TrafficPolicyInstance"
               (Util.option_bind (Xml.member "TrafficPolicyInstance" xml)
                  TrafficPolicyInstance.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicyInstance",
                   (TrafficPolicyInstance.to_query v.traffic_policy_instance)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("traffic_policy_instance",
                (TrafficPolicyInstance.to_json v.traffic_policy_instance))])
    let of_json j =
      {
        traffic_policy_instance =
          (TrafficPolicyInstance.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instance")))
      }
  end
module DeleteVPCAssociationAuthorizationResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GetHealthCheckCountRequest =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsufficientCloudWatchLogsResourcePolicy =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateTrafficPolicyVersionRequest =
  struct
    type t = {
      id: String.t ;
      document: String.t ;
      comment: String.t option }
    let make ~id  ~document  ?comment  () = { id; document; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          document =
            (Xml.required "Document"
               (Util.option_bind (Xml.member "Document" xml) String.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("Document", (String.to_query v.document)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("document", (String.to_json v.document));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        document =
          (String.of_json (Util.of_option_exn (Json.lookup j "document")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module DelegationSetInUse =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTrafficPolicyInstancesRequest =
  struct
    type t =
      {
      hosted_zone_id_marker: String.t option ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      max_items: String.t option }
    let make ?hosted_zone_id_marker  ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ?max_items  () =
      {
        hosted_zone_id_marker;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        max_items
      }
    let parse xml =
      Some
        {
          hosted_zone_id_marker =
            (Util.option_bind (Xml.member "hostedzoneid" xml) String.parse);
          traffic_policy_instance_name_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancename" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind (Xml.member "trafficpolicyinstancetype" xml)
               RRType.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancetype", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair ("trafficpolicyinstancename", (String.to_query f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> Query.Pair ("hostedzoneid", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> ("hosted_zone_id_marker", (String.to_json f)))])
    let of_json j =
      {
        hosted_zone_id_marker =
          (Util.option_map (Json.lookup j "hosted_zone_id_marker")
             String.of_json);
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module GetReusableDelegationSetRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module TrafficPolicyInstanceAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DeleteHostedZoneResponse =
  struct
    type t = {
      change_info: ChangeInfo.t }
    let make ~change_info  () = { change_info }
    let parse xml =
      Some
        {
          change_info =
            (Xml.required "ChangeInfo"
               (Util.option_bind (Xml.member "ChangeInfo" xml)
                  ChangeInfo.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ChangeInfo", (ChangeInfo.to_query v.change_info)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_info", (ChangeInfo.to_json v.change_info))])
    let of_json j =
      {
        change_info =
          (ChangeInfo.of_json
             (Util.of_option_exn (Json.lookup j "change_info")))
      }
  end
module DeleteTrafficPolicyResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListTrafficPolicyInstancesByHostedZoneResponse =
  struct
    type t =
      {
      traffic_policy_instances: TrafficPolicyInstances.t ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      is_truncated: Boolean.t ;
      max_items: String.t }
    let make ~traffic_policy_instances  ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ~is_truncated  ~max_items  () =
      {
        traffic_policy_instances;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        is_truncated;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policy_instances =
            (Xml.required "TrafficPolicyInstances"
               (Util.option_bind (Xml.member "TrafficPolicyInstances" xml)
                  TrafficPolicyInstances.parse));
          traffic_policy_instance_name_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceNameMarker" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceTypeMarker" xml)
               RRType.parse);
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceTypeMarker", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceNameMarker", (String.to_query f)));
           Some
             (Query.Pair
                ("TrafficPolicyInstances.member",
                  (TrafficPolicyInstances.to_query v.traffic_policy_instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Some
             ("traffic_policy_instances",
               (TrafficPolicyInstances.to_json v.traffic_policy_instances))])
    let of_json j =
      {
        traffic_policy_instances =
          (TrafficPolicyInstances.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instances")));
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module NoSuchHealthCheck =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module NoSuchTrafficPolicy =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DelegationSetNotReusable =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module GetReusableDelegationSetLimitResponse =
  struct
    type t = {
      limit: ReusableDelegationSetLimit.t ;
      count: Long.t }
    let make ~limit  ~count  () = { limit; count }
    let parse xml =
      Some
        {
          limit =
            (Xml.required "Limit"
               (Util.option_bind (Xml.member "Limit" xml)
                  ReusableDelegationSetLimit.parse));
          count =
            (Xml.required "Count"
               (Util.option_bind (Xml.member "Count" xml) Long.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Count", (Long.to_query v.count)));
           Some
             (Query.Pair
                ("Limit", (ReusableDelegationSetLimit.to_query v.limit)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("count", (Long.to_json v.count));
           Some ("limit", (ReusableDelegationSetLimit.to_json v.limit))])
    let of_json j =
      {
        limit =
          (ReusableDelegationSetLimit.of_json
             (Util.of_option_exn (Json.lookup j "limit")));
        count = (Long.of_json (Util.of_option_exn (Json.lookup j "count")))
      }
  end
module HealthCheckVersionMismatch =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module InvalidDomainName =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListReusableDelegationSetsResponse =
  struct
    type t =
      {
      delegation_sets: DelegationSets.t ;
      marker: String.t ;
      is_truncated: Boolean.t ;
      next_marker: String.t option ;
      max_items: String.t }
    let make ~delegation_sets  ~marker  ~is_truncated  ?next_marker 
      ~max_items  () =
      { delegation_sets; marker; is_truncated; next_marker; max_items }
    let parse xml =
      Some
        {
          delegation_sets =
            (Xml.required "DelegationSets"
               (Util.option_bind (Xml.member "DelegationSets" xml)
                  DelegationSets.parse));
          marker =
            (Xml.required "Marker"
               (Util.option_bind (Xml.member "Marker" xml) String.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse);
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Util.option_map v.next_marker
             (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some (Query.Pair ("Marker", (String.to_query v.marker)));
           Some
             (Query.Pair
                ("DelegationSets.member",
                  (DelegationSets.to_query v.delegation_sets)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Util.option_map v.next_marker
             (fun f -> ("next_marker", (String.to_json f)));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some ("marker", (String.to_json v.marker));
           Some
             ("delegation_sets", (DelegationSets.to_json v.delegation_sets))])
    let of_json j =
      {
        delegation_sets =
          (DelegationSets.of_json
             (Util.of_option_exn (Json.lookup j "delegation_sets")));
        marker =
          (String.of_json (Util.of_option_exn (Json.lookup j "marker")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json);
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module DeleteHealthCheckResponse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateHostedZoneCommentResponse =
  struct
    type t = {
      hosted_zone: HostedZone.t }
    let make ~hosted_zone  () = { hosted_zone }
    let parse xml =
      Some
        {
          hosted_zone =
            (Xml.required "HostedZone"
               (Util.option_bind (Xml.member "HostedZone" xml)
                  HostedZone.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("HostedZone", (HostedZone.to_query v.hosted_zone)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("hosted_zone", (HostedZone.to_json v.hosted_zone))])
    let of_json j =
      {
        hosted_zone =
          (HostedZone.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone")))
      }
  end
module NoSuchQueryLoggingConfig =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TrafficPolicyInUse =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module UpdateTrafficPolicyCommentResponse =
  struct
    type t = {
      traffic_policy: TrafficPolicy.t }
    let make ~traffic_policy  () = { traffic_policy }
    let parse xml =
      Some
        {
          traffic_policy =
            (Xml.required "TrafficPolicy"
               (Util.option_bind (Xml.member "TrafficPolicy" xml)
                  TrafficPolicy.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TrafficPolicy", (TrafficPolicy.to_query v.traffic_policy)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("traffic_policy", (TrafficPolicy.to_json v.traffic_policy))])
    let of_json j =
      {
        traffic_policy =
          (TrafficPolicy.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy")))
      }
  end
module CreateHealthCheckRequest =
  struct
    type t =
      {
      caller_reference: String.t ;
      health_check_config: HealthCheckConfig.t }
    let make ~caller_reference  ~health_check_config  () =
      { caller_reference; health_check_config }
    let parse xml =
      Some
        {
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          health_check_config =
            (Xml.required "HealthCheckConfig"
               (Util.option_bind (Xml.member "HealthCheckConfig" xml)
                  HealthCheckConfig.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckConfig",
                   (HealthCheckConfig.to_query v.health_check_config)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("health_check_config",
                (HealthCheckConfig.to_json v.health_check_config));
           Some ("caller_reference", (String.to_json v.caller_reference))])
    let of_json j =
      {
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        health_check_config =
          (HealthCheckConfig.of_json
             (Util.of_option_exn (Json.lookup j "health_check_config")))
      }
  end
module CreateHostedZoneRequest =
  struct
    type t =
      {
      name: String.t ;
      v_p_c: VPC.t option ;
      caller_reference: String.t ;
      hosted_zone_config: HostedZoneConfig.t option ;
      delegation_set_id: String.t option }
    let make ~name  ?v_p_c  ~caller_reference  ?hosted_zone_config 
      ?delegation_set_id  () =
      { name; v_p_c; caller_reference; hosted_zone_config; delegation_set_id
      }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          v_p_c = (Util.option_bind (Xml.member "VPC" xml) VPC.parse);
          caller_reference =
            (Xml.required "CallerReference"
               (Util.option_bind (Xml.member "CallerReference" xml)
                  String.parse));
          hosted_zone_config =
            (Util.option_bind (Xml.member "HostedZoneConfig" xml)
               HostedZoneConfig.parse);
          delegation_set_id =
            (Util.option_bind (Xml.member "DelegationSetId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.delegation_set_id
              (fun f -> Query.Pair ("DelegationSetId", (String.to_query f)));
           Util.option_map v.hosted_zone_config
             (fun f ->
                Query.Pair
                  ("HostedZoneConfig", (HostedZoneConfig.to_query f)));
           Some
             (Query.Pair
                ("CallerReference", (String.to_query v.caller_reference)));
           Util.option_map v.v_p_c
             (fun f -> Query.Pair ("VPC", (VPC.to_query f)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.delegation_set_id
              (fun f -> ("delegation_set_id", (String.to_json f)));
           Util.option_map v.hosted_zone_config
             (fun f -> ("hosted_zone_config", (HostedZoneConfig.to_json f)));
           Some ("caller_reference", (String.to_json v.caller_reference));
           Util.option_map v.v_p_c (fun f -> ("v_p_c", (VPC.to_json f)));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        v_p_c = (Util.option_map (Json.lookup j "v_p_c") VPC.of_json);
        caller_reference =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "caller_reference")));
        hosted_zone_config =
          (Util.option_map (Json.lookup j "hosted_zone_config")
             HostedZoneConfig.of_json);
        delegation_set_id =
          (Util.option_map (Json.lookup j "delegation_set_id") String.of_json)
      }
  end
module DeleteTrafficPolicyInstanceRequest =
  struct
    type t = {
      id: String.t }
    let make ~id  () = { id }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc (Util.list_filter_opt [Some ("id", (String.to_json v.id))])
    let of_json j =
      { id = (String.of_json (Util.of_option_exn (Json.lookup j "id"))) }
  end
module GetTrafficPolicyRequest =
  struct
    type t = {
      id: String.t ;
      version: Integer.t }
    let make ~id  ~version  () = { id; version }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          version =
            (Xml.required "Version"
               (Util.option_bind (Xml.member "Version" xml) Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Version", (Integer.to_query v.version)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("version", (Integer.to_json v.version));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        version =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "version")))
      }
  end
module CreateTrafficPolicyVersionResponse =
  struct
    type t = {
      traffic_policy: TrafficPolicy.t ;
      location: String.t }
    let make ~traffic_policy  ~location  () = { traffic_policy; location }
    let parse xml =
      Some
        {
          traffic_policy =
            (Xml.required "TrafficPolicy"
               (Util.option_bind (Xml.member "TrafficPolicy" xml)
                  TrafficPolicy.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("TrafficPolicy", (TrafficPolicy.to_query v.traffic_policy)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some ("traffic_policy", (TrafficPolicy.to_json v.traffic_policy))])
    let of_json j =
      {
        traffic_policy =
          (TrafficPolicy.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module CreateTrafficPolicyRequest =
  struct
    type t = {
      name: String.t ;
      document: String.t ;
      comment: String.t option }
    let make ~name  ~document  ?comment  () = { name; document; comment }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          document =
            (Xml.required "Document"
               (Util.option_bind (Xml.member "Document" xml) String.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("Document", (String.to_query v.document)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("document", (String.to_json v.document));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        document =
          (String.of_json (Util.of_option_exn (Json.lookup j "document")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module DeleteHealthCheckRequest =
  struct
    type t = {
      health_check_id: String.t }
    let make ~health_check_id  () = { health_check_id }
    let parse xml =
      Some
        {
          health_check_id =
            (Xml.required "HealthCheckId"
               (Util.option_bind (Xml.member "HealthCheckId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheckId", (String.to_query v.health_check_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check_id", (String.to_json v.health_check_id))])
    let of_json j =
      {
        health_check_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "health_check_id")))
      }
  end
module LastVPCAssociation =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTrafficPoliciesResponse =
  struct
    type t =
      {
      traffic_policy_summaries: TrafficPolicySummaries.t ;
      is_truncated: Boolean.t ;
      traffic_policy_id_marker: String.t ;
      max_items: String.t }
    let make ~traffic_policy_summaries  ~is_truncated 
      ~traffic_policy_id_marker  ~max_items  () =
      {
        traffic_policy_summaries;
        is_truncated;
        traffic_policy_id_marker;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policy_summaries =
            (Xml.required "TrafficPolicySummaries"
               (Util.option_bind (Xml.member "TrafficPolicySummaries" xml)
                  TrafficPolicySummaries.parse));
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          traffic_policy_id_marker =
            (Xml.required "TrafficPolicyIdMarker"
               (Util.option_bind (Xml.member "TrafficPolicyIdMarker" xml)
                  String.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair
                ("TrafficPolicyIdMarker",
                  (String.to_query v.traffic_policy_id_marker)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Some
             (Query.Pair
                ("TrafficPolicySummaries.member",
                  (TrafficPolicySummaries.to_query v.traffic_policy_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Some
             ("traffic_policy_id_marker",
               (String.to_json v.traffic_policy_id_marker));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Some
             ("traffic_policy_summaries",
               (TrafficPolicySummaries.to_json v.traffic_policy_summaries))])
    let of_json j =
      {
        traffic_policy_summaries =
          (TrafficPolicySummaries.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_summaries")));
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        traffic_policy_id_marker =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_id_marker")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module PriorRequestNotComplete =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module HostedZoneNotPrivate =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ChangeResourceRecordSetsRequest =
  struct
    type t = {
      hosted_zone_id: String.t ;
      change_batch: ChangeBatch.t }
    let make ~hosted_zone_id  ~change_batch  () =
      { hosted_zone_id; change_batch }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          change_batch =
            (Xml.required "ChangeBatch"
               (Util.option_bind (Xml.member "ChangeBatch" xml)
                  ChangeBatch.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ChangeBatch", (ChangeBatch.to_query v.change_batch)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("change_batch", (ChangeBatch.to_json v.change_batch));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        change_batch =
          (ChangeBatch.of_json
             (Util.of_option_exn (Json.lookup j "change_batch")))
      }
  end
module GetHostedZoneResponse =
  struct
    type t =
      {
      hosted_zone: HostedZone.t ;
      delegation_set: DelegationSet.t option ;
      v_p_cs: VPCs.t }
    let make ~hosted_zone  ?delegation_set  ?(v_p_cs= [])  () =
      { hosted_zone; delegation_set; v_p_cs }
    let parse xml =
      Some
        {
          hosted_zone =
            (Xml.required "HostedZone"
               (Util.option_bind (Xml.member "HostedZone" xml)
                  HostedZone.parse));
          delegation_set =
            (Util.option_bind (Xml.member "DelegationSet" xml)
               DelegationSet.parse);
          v_p_cs =
            (Util.of_option []
               (Util.option_bind (Xml.member "VPCs" xml) VPCs.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("VPCs.member", (VPCs.to_query v.v_p_cs)));
           Util.option_map v.delegation_set
             (fun f ->
                Query.Pair ("DelegationSet", (DelegationSet.to_query f)));
           Some
             (Query.Pair ("HostedZone", (HostedZone.to_query v.hosted_zone)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("v_p_cs", (VPCs.to_json v.v_p_cs));
           Util.option_map v.delegation_set
             (fun f -> ("delegation_set", (DelegationSet.to_json f)));
           Some ("hosted_zone", (HostedZone.to_json v.hosted_zone))])
    let of_json j =
      {
        hosted_zone =
          (HostedZone.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone")));
        delegation_set =
          (Util.option_map (Json.lookup j "delegation_set")
             DelegationSet.of_json);
        v_p_cs = (VPCs.of_json (Util.of_option_exn (Json.lookup j "v_p_cs")))
      }
  end
module CreateTrafficPolicyResponse =
  struct
    type t = {
      traffic_policy: TrafficPolicy.t ;
      location: String.t }
    let make ~traffic_policy  ~location  () = { traffic_policy; location }
    let parse xml =
      Some
        {
          traffic_policy =
            (Xml.required "TrafficPolicy"
               (Util.option_bind (Xml.member "TrafficPolicy" xml)
                  TrafficPolicy.parse));
          location =
            (Xml.required "Location"
               (Util.option_bind (Xml.member "Location" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Location", (String.to_query v.location)));
           Some
             (Query.Pair
                ("TrafficPolicy", (TrafficPolicy.to_query v.traffic_policy)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("location", (String.to_json v.location));
           Some ("traffic_policy", (TrafficPolicy.to_json v.traffic_policy))])
    let of_json j =
      {
        traffic_policy =
          (TrafficPolicy.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy")));
        location =
          (String.of_json (Util.of_option_exn (Json.lookup j "location")))
      }
  end
module HostedZoneAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ListTrafficPolicyInstancesByPolicyResponse =
  struct
    type t =
      {
      traffic_policy_instances: TrafficPolicyInstances.t ;
      hosted_zone_id_marker: String.t option ;
      traffic_policy_instance_name_marker: String.t option ;
      traffic_policy_instance_type_marker: RRType.t option ;
      is_truncated: Boolean.t ;
      max_items: String.t }
    let make ~traffic_policy_instances  ?hosted_zone_id_marker 
      ?traffic_policy_instance_name_marker 
      ?traffic_policy_instance_type_marker  ~is_truncated  ~max_items  () =
      {
        traffic_policy_instances;
        hosted_zone_id_marker;
        traffic_policy_instance_name_marker;
        traffic_policy_instance_type_marker;
        is_truncated;
        max_items
      }
    let parse xml =
      Some
        {
          traffic_policy_instances =
            (Xml.required "TrafficPolicyInstances"
               (Util.option_bind (Xml.member "TrafficPolicyInstances" xml)
                  TrafficPolicyInstances.parse));
          hosted_zone_id_marker =
            (Util.option_bind (Xml.member "HostedZoneIdMarker" xml)
               String.parse);
          traffic_policy_instance_name_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceNameMarker" xml)
               String.parse);
          traffic_policy_instance_type_marker =
            (Util.option_bind
               (Xml.member "TrafficPolicyInstanceTypeMarker" xml)
               RRType.parse);
          is_truncated =
            (Xml.required "IsTruncated"
               (Util.option_bind (Xml.member "IsTruncated" xml) Boolean.parse));
          max_items =
            (Xml.required "MaxItems"
               (Util.option_bind (Xml.member "MaxItems" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("MaxItems", (String.to_query v.max_items)));
           Some
             (Query.Pair ("IsTruncated", (Boolean.to_query v.is_truncated)));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceTypeMarker", (RRType.to_query f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                Query.Pair
                  ("TrafficPolicyInstanceNameMarker", (String.to_query f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> Query.Pair ("HostedZoneIdMarker", (String.to_query f)));
           Some
             (Query.Pair
                ("TrafficPolicyInstances.member",
                  (TrafficPolicyInstances.to_query v.traffic_policy_instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("max_items", (String.to_json v.max_items));
           Some ("is_truncated", (Boolean.to_json v.is_truncated));
           Util.option_map v.traffic_policy_instance_type_marker
             (fun f ->
                ("traffic_policy_instance_type_marker", (RRType.to_json f)));
           Util.option_map v.traffic_policy_instance_name_marker
             (fun f ->
                ("traffic_policy_instance_name_marker", (String.to_json f)));
           Util.option_map v.hosted_zone_id_marker
             (fun f -> ("hosted_zone_id_marker", (String.to_json f)));
           Some
             ("traffic_policy_instances",
               (TrafficPolicyInstances.to_json v.traffic_policy_instances))])
    let of_json j =
      {
        traffic_policy_instances =
          (TrafficPolicyInstances.of_json
             (Util.of_option_exn (Json.lookup j "traffic_policy_instances")));
        hosted_zone_id_marker =
          (Util.option_map (Json.lookup j "hosted_zone_id_marker")
             String.of_json);
        traffic_policy_instance_name_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_name_marker")
             String.of_json);
        traffic_policy_instance_type_marker =
          (Util.option_map
             (Json.lookup j "traffic_policy_instance_type_marker")
             RRType.of_json);
        is_truncated =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "is_truncated")));
        max_items =
          (String.of_json (Util.of_option_exn (Json.lookup j "max_items")))
      }
  end
module ListHostedZonesByNameRequest =
  struct
    type t =
      {
      d_n_s_name: String.t option ;
      hosted_zone_id: String.t option ;
      max_items: String.t option }
    let make ?d_n_s_name  ?hosted_zone_id  ?max_items  () =
      { d_n_s_name; hosted_zone_id; max_items }
    let parse xml =
      Some
        {
          d_n_s_name =
            (Util.option_bind (Xml.member "dnsname" xml) String.parse);
          hosted_zone_id =
            (Util.option_bind (Xml.member "hostedzoneid" xml) String.parse);
          max_items =
            (Util.option_bind (Xml.member "maxitems" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> Query.Pair ("maxitems", (String.to_query f)));
           Util.option_map v.hosted_zone_id
             (fun f -> Query.Pair ("hostedzoneid", (String.to_query f)));
           Util.option_map v.d_n_s_name
             (fun f -> Query.Pair ("dnsname", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_items
              (fun f -> ("max_items", (String.to_json f)));
           Util.option_map v.hosted_zone_id
             (fun f -> ("hosted_zone_id", (String.to_json f)));
           Util.option_map v.d_n_s_name
             (fun f -> ("d_n_s_name", (String.to_json f)))])
    let of_json j =
      {
        d_n_s_name =
          (Util.option_map (Json.lookup j "d_n_s_name") String.of_json);
        hosted_zone_id =
          (Util.option_map (Json.lookup j "hosted_zone_id") String.of_json);
        max_items =
          (Util.option_map (Json.lookup j "max_items") String.of_json)
      }
  end
module UpdateHostedZoneCommentRequest =
  struct
    type t = {
      id: String.t ;
      comment: String.t option }
    let make ~id  ?comment  () = { id; comment }
    let parse xml =
      Some
        {
          id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          comment =
            (Util.option_bind (Xml.member "Comment" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> Query.Pair ("Comment", (String.to_query f)));
           Some (Query.Pair ("Id", (String.to_query v.id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.comment
              (fun f -> ("comment", (String.to_json f)));
           Some ("id", (String.to_json v.id))])
    let of_json j =
      {
        id = (String.of_json (Util.of_option_exn (Json.lookup j "id")));
        comment = (Util.option_map (Json.lookup j "comment") String.of_json)
      }
  end
module ConflictingTypes =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module CreateVPCAssociationAuthorizationRequest =
  struct
    type t = {
      hosted_zone_id: String.t ;
      v_p_c: VPC.t }
    let make ~hosted_zone_id  ~v_p_c  () = { hosted_zone_id; v_p_c }
    let parse xml =
      Some
        {
          hosted_zone_id =
            (Xml.required "Id"
               (Util.option_bind (Xml.member "Id" xml) String.parse));
          v_p_c =
            (Xml.required "VPC"
               (Util.option_bind (Xml.member "VPC" xml) VPC.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("VPC", (VPC.to_query v.v_p_c)));
           Some (Query.Pair ("Id", (String.to_query v.hosted_zone_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("v_p_c", (VPC.to_json v.v_p_c));
           Some ("hosted_zone_id", (String.to_json v.hosted_zone_id))])
    let of_json j =
      {
        hosted_zone_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "hosted_zone_id")));
        v_p_c = (VPC.of_json (Util.of_option_exn (Json.lookup j "v_p_c")))
      }
  end
module ConcurrentModification =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module HealthCheckAlreadyExists =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TestDNSAnswerResponse =
  struct
    type t =
      {
      nameserver: String.t ;
      record_name: String.t ;
      record_type: RRType.t ;
      record_data: RecordData.t ;
      response_code: String.t ;
      protocol: String.t }
    let make ~nameserver  ~record_name  ~record_type  ~record_data 
      ~response_code  ~protocol  () =
      {
        nameserver;
        record_name;
        record_type;
        record_data;
        response_code;
        protocol
      }
    let parse xml =
      Some
        {
          nameserver =
            (Xml.required "Nameserver"
               (Util.option_bind (Xml.member "Nameserver" xml) String.parse));
          record_name =
            (Xml.required "RecordName"
               (Util.option_bind (Xml.member "RecordName" xml) String.parse));
          record_type =
            (Xml.required "RecordType"
               (Util.option_bind (Xml.member "RecordType" xml) RRType.parse));
          record_data =
            (Xml.required "RecordData"
               (Util.option_bind (Xml.member "RecordData" xml)
                  RecordData.parse));
          response_code =
            (Xml.required "ResponseCode"
               (Util.option_bind (Xml.member "ResponseCode" xml) String.parse));
          protocol =
            (Xml.required "Protocol"
               (Util.option_bind (Xml.member "Protocol" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Protocol", (String.to_query v.protocol)));
           Some
             (Query.Pair ("ResponseCode", (String.to_query v.response_code)));
           Some
             (Query.Pair
                ("RecordData.member", (RecordData.to_query v.record_data)));
           Some (Query.Pair ("RecordType", (RRType.to_query v.record_type)));
           Some (Query.Pair ("RecordName", (String.to_query v.record_name)));
           Some (Query.Pair ("Nameserver", (String.to_query v.nameserver)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("protocol", (String.to_json v.protocol));
           Some ("response_code", (String.to_json v.response_code));
           Some ("record_data", (RecordData.to_json v.record_data));
           Some ("record_type", (RRType.to_json v.record_type));
           Some ("record_name", (String.to_json v.record_name));
           Some ("nameserver", (String.to_json v.nameserver))])
    let of_json j =
      {
        nameserver =
          (String.of_json (Util.of_option_exn (Json.lookup j "nameserver")));
        record_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "record_name")));
        record_type =
          (RRType.of_json (Util.of_option_exn (Json.lookup j "record_type")));
        record_data =
          (RecordData.of_json
             (Util.of_option_exn (Json.lookup j "record_data")));
        response_code =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "response_code")));
        protocol =
          (String.of_json (Util.of_option_exn (Json.lookup j "protocol")))
      }
  end