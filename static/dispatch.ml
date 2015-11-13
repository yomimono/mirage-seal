open V1
open V1_LWT

let (>>=) = Lwt.bind

(* HTTP handler *)
module type HTTP = sig
  include Cohttp_lwt.Server
  val listen: t -> IO.conn -> unit Lwt.t
end

module Dispatch (C: CONSOLE) (FS: KV_RO) (S: HTTP) = struct

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let read_fs fs name =
    FS.size fs name >>= function
    | `Error (FS.Unknown_key _) ->
      Lwt.fail (Failure ("read " ^ name))
    | `Ok size ->
      FS.read fs name 0 (Int64.to_int size) >>= function
      | `Error (FS.Unknown_key _) -> Lwt.fail (Failure ("read " ^ name))
      | `Ok bufs -> Lwt.return (Cstruct.copyv bufs)

  (* dispatch files *)
  let rec dispatcher fs ?header uri = match Uri.path uri with
    | "" | "/" -> dispatcher fs ?header (Uri.with_path uri "index.html")
    | path ->
      let mimetype = Magic_mime.lookup path in
      let headers = Cohttp.Header.add_opt header "content-type" mimetype in
      Lwt.catch
        (fun () ->
           read_fs fs path >>= fun body ->
           S.respond_string ~status:`OK ~body ~headers ())
        (fun exn ->
           S.respond_not_found ())

  (* Redirect to the same address, but in https. *)
  let redirect uri =
    let new_uri = Uri.with_scheme uri (Some "https") in
    let headers =
      Cohttp.Header.init_with "location" (Uri.to_string new_uri)
    in
    S.respond ~headers ~status:`Moved_permanently ~body:`Empty ()

  let serve c flow f =
    let callback (_, cid) request body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      log c "[%s] serving %s." cid (Uri.to_string uri);
      f uri
    in
    let conn_closed (_,cid) =
      let cid = Cohttp.Connection.to_string cid in
      log c "[%s] closing." cid
    in
    let http = S.make ~conn_closed ~callback () in
    S.listen http flow

end

(* HTTP *)
module HTTP
    (C : CONSOLE) (S : STACKV4)
    (DATA : KV_RO)
    (Clock : CLOCK) =
struct

  module Http     = Cohttp_mirage.Server(S.TCPV4)
  module Dispatch = Dispatch(C)(DATA)(Http)

  let start c stack data _clock =
    let http flow = Dispatch.serve c flow (Dispatch.dispatcher data) in
    S.listen_tcpv4 stack ~port:80 http;
    S.listen stack

end

