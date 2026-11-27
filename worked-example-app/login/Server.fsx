#load "Schemas.fsx"
open System
open System.IO
open System.Net
open Schemas

let users = Map [ "alice", "wonderland"; "bob", "builder" ]

let newToken () : string = Guid.NewGuid().ToString "N"

let succeed (username: string) : int * string =
    let reply = Token.Create(username = username, token = newToken ())
    200, string reply.JsonVal

let fail (status: int) (first: string) (rest: string list) : int * string =
    let reply = Failure.Create(messages = (first, rest))
    status, string reply.JsonVal

let logIn (login: Login) : int * string =
    match Map.tryFind login.username users with
    | Some password when password = login.password -> succeed login.username
    | _ -> fail 401 "wrong username or password" []

let handle (body: string) : int * string =
    match Login.Parse body with
    | Ok login -> logIn login
    | Error errors -> fail 400 "invalid request" errors

let respond (context: HttpListenerContext) : unit =
    use reader = new StreamReader(context.Request.InputStream)
    let request = reader.ReadToEnd()
    let status, answer = handle request
    printfn "request:  %s" request
    printfn "response: %d %s\n" status answer
    let bytes = Text.Encoding.UTF8.GetBytes answer
    context.Response.StatusCode <- status
    context.Response.OutputStream.Write(bytes, 0, bytes.Length)
    context.Response.Close()

let listener = new HttpListener()
listener.Prefixes.Add url
listener.Start()
printfn "listening on %s" url

while true do
    respond (listener.GetContext())
