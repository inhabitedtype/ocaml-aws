open Aws_cloudtrail_test

module T = TestSuite(struct
    type 'a m = 'a Lwt.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY"
    let secret_key = Unix.getenv "AWS_SECRET_KEY"

    let run_request = Aws_lwt.Runtime.run_request ~access_key ~secret_key
    let un_m = Lwt_main.run
  end)
