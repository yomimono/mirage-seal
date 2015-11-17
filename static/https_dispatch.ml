open V1
open V1_LWT

let (>>=) = Lwt.bind

(* HTTPS *)
module HTTPS
    (C : CONSOLE) (S : STACKV4)
    (DATA : KV_RO) (KEYS: KV_RO)
    (Clock : CLOCK) =
struct

  module TCP  = S.TCPV4
  module TLS  = Tls_mirage.Make (TCP)
  module X509 = Tls_mirage.X509 (KEYS) (Clock)

  module Http  = Cohttp_mirage.Server(TCP)
  module Https = Cohttp_mirage.Server(TLS)

  module Dispatch_http  = Dispatch.Dispatch(C)(DATA)(Http)
  module Dispatch_https = Dispatch.Dispatch(C)(DATA)(Https)

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let with_tls c cfg tcp ~f =
    let peer, port = TCP.get_dest tcp in
    let log str = log c "[%s:%d] %s" (Ipaddr.V4.to_string peer) port str in
    let with_tls_server k = TLS.server_of_flow cfg tcp >>= k in
    with_tls_server @@ function
    | `Error _ -> log "TLS failed"; TCP.close tcp
    | `Ok tls  -> log "TLS ok"; f tls >>= fun () -> TLS.close tls
    | `Eof     -> log "TLS eof"; TCP.close tcp

  let tls_init kv =
    X509.certificate kv `Default >>= fun cert ->
    let conf = Tls.Config.server ~certificates:(`Single cert) () in
    Lwt.return conf

  let start c stack data keys _clock =
    tls_init keys >>= fun cfg ->
    (* 31536000 seconds is roughly a year *)
    let header = Cohttp.Header.init_with "Strict-Transport-Security" "max-age=31536000" in
    let https flow = Dispatch_https.serve c flow (Dispatch_https.dispatcher ~header data) in
    let http  flow = Dispatch_http.serve  c flow (Dispatch_http.dispatcher ~header data) in
    S.listen_tcpv4 stack ~port:443 (with_tls c cfg ~f:https);
    S.listen_tcpv4 stack ~port:80  http;
    S.listen stack

end
