#load "Schemas.fsx"
open System
open System.Net.Http
open RefinedJsonSchemaProvider
open Schemas

[<Literal>]
let replySchema =
    """{ "oneOf": [ """ + tokenSchema + ", " + failureSchema + " ] }"

type Reply =
    RefinedJsonSchemaProvider<schema = replySchema, allowNullStrings = true>

let client = new HttpClient()

let printReply (status: int) (answer: string) : unit =
    match Reply.Parse answer with
    | Ok (Choice1Of2 token) ->
        printfn "Hello %s! (%d)" token.username status
        printfn "Your token is %s" token.token
    | Ok (Choice2Of2 failure) ->
        let first, rest = failure.messages
        printfn "Refused (%d):" status
        for message in first :: rest do
            printfn "  %s" message
    | Error _ -> printfn "Unexpected answer: %s" answer

let send (login: Login) : unit =
    use content = new StringContent(string login.JsonVal)
    try
        let response = client.PostAsync(url, content).Result
        let answer = response.Content.ReadAsStringAsync().Result
        printReply (int response.StatusCode) answer
    with :? AggregateException ->
        printfn "No server on %s, start Server.fsx first" url

let ask (label: string) : string =
    printf "%s: " label
    Console.ReadLine()

let rec readHidden (typed: string) : string =
    let key = Console.ReadKey true
    match key.Key with
    | ConsoleKey.Enter ->
        printfn ""
        typed
    | ConsoleKey.Backspace when typed.Length > 0 ->
        printf "\b \b"
        readHidden (typed.Substring(0, typed.Length - 1))
    | ConsoleKey.Backspace -> readHidden typed
    | _ ->
        printf "*"
        readHidden (typed + string key.KeyChar)

let askHidden (label: string) : string =
    if Console.IsInputRedirected then
        ask label
    else
        printf "%s: " label
        readHidden ""

let logIn () : unit =
    let username = ask "Username"
    let password = askHidden "Password"
    match Login.Create(username = username, password = password) with
    | Ok login -> send login
    | Error errors -> printfn "Not sent: %A" errors

let rec menu () : unit =
    printfn ""
    printfn "1) Log in"
    printfn "2) Quit"
    match ask "Choose" with
    | null
    | "2" -> ()
    | "1" ->
        logIn ()
        menu ()
    | _ ->
        printfn "Choose 1 or 2"
        menu ()

printfn "Users: alice / wonderland and bob / builder"
menu ()
