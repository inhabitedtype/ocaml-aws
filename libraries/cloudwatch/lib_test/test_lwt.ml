open Aws_cloudwatch_test

module T = TestSuite(struct
    type 'a m = 'a Lwt.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY_ID"
    let secret_key = Unix.getenv "AWS_SECRET_ACCESS_KEY"
    let region = Unix.getenv "AWS_DEFAULT_REGION"

    let run_request x = Aws_lwt.Runtime.run_request ~region ~access_key ~secret_key x
    let un_m = Lwt_main.run
  end)
