module Fable.IcoMoonProvider

module internal IO =
    // File watcher implementation taken from FSharp.Data
    open System
    open System.IO
    open System.Collections.Generic

    type private FileWatcher(path) =

        let subscriptions = Dictionary<string, unit -> unit>()

        let getLastWrite() = File.GetLastWriteTime path
        let mutable lastWrite = getLastWrite()

        let watcher =
            new FileSystemWatcher(
                Filter = Path.GetFileName path,
                Path = Path.GetDirectoryName path,
                EnableRaisingEvents = true)

        let checkForChanges action _ =
            let curr = getLastWrite()

            if lastWrite <> curr then
                // log (sprintf "File %s: %s" action path)
                lastWrite <- curr
                // creating a copy since the handler can be unsubscribed during the iteration
                let handlers = subscriptions.Values |> Seq.toArray
                for handler in handlers do
                    handler()

        do
            watcher.Changed.Add (checkForChanges "changed")
            watcher.Renamed.Add (checkForChanges "renamed")
            watcher.Deleted.Add (checkForChanges "deleted")

        member __.Subscribe(name, action) =
            subscriptions.Add(name, action)

        member __.Unsubscribe(name) =
            if subscriptions.Remove(name) then
                // log (sprintf "Unsubscribed %s from %s watcher" name path)
                if subscriptions.Count = 0 then
                    // log (sprintf "Disposing %s watcher" path)
                    watcher.Dispose()
                    true
                else
                    false
            else
                false

    let private watchers = Dictionary<string, FileWatcher>()

    // sets up a filesystem watcher that calls the invalidate function whenever the file changes
    let watchForChanges path (owner, onChange) =

        let watcher =

            lock watchers <| fun () ->

                match watchers.TryGetValue(path) with
                | true, watcher ->

                    // log (sprintf "Reusing %s watcher" path)
                    watcher.Subscribe(owner, onChange)
                    watcher

                | false, _ ->

                    // log (sprintf "Setting up %s watcher" path)
                    let watcher = FileWatcher path
                    watcher.Subscribe(owner, onChange)
                    watchers.Add(path, watcher)
                    watcher

        { new IDisposable with
            member __.Dispose() =
                lock watchers <| fun () ->
                    if watcher.Unsubscribe(owner) then
                        watchers.Remove(path) |> ignore
        }


open System.Text.RegularExpressions
open FSharp.Quotations
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
// open Newtonsoft.Json.Linq

open ProviderDsl
open Fable.SimpleHttp


// let sanitizeName (name:string) =
//     name.Replace('-', '_')

let convertChar (name:string, char:string) =
    Property(name, String, true, (fun _args -> <@@ char @@>) )

let convertChars asm ns typeName chars =
    makeRootType(asm, ns, typeName, chars |> List.map convertChar)


[<TypeProvider>]
type public IcoMoonProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation = true)
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "Fable.IcoMoonProvider"

    let staticParams = [ProvidedStaticParameter("selectionFilePath",typeof<string>)]
    let generator = ProvidedTypeDefinition(asm, ns, "Generator", Some typeof<obj>, isErased = true)

    let watcherSubscriptions = System.Collections.Concurrent.ConcurrentDictionary<string, System.IDisposable>()

    let tryFindObjectProperty nameToFind (json:JsonParser.Json) =
        match json with
        | JsonParser.Json.Object (properties) ->
            properties
            |> List.tryPick (fun (name, value) -> if name = nameToFind then Some value else None )
        | _ -> None

    let tryAsString (json:JsonParser.Json) =
        match json with
        | JsonParser.Json.String value -> Some value
        | _ -> None

    let tryAsNumber (json:JsonParser.Json) =
        match json with
        | JsonParser.Json.Number value -> Some value
        | _ -> None

    let tryAsArray (json:JsonParser.Json) =
        match json with
        | JsonParser.Json.Array value -> Some value
        | _ -> None

    let getFontPrefix (json:JsonParser.Json) =
        json
        |> tryFindObjectProperty "preferences"
        |> Option.bind (tryFindObjectProperty "fontPref")
        |> Option.bind (tryFindObjectProperty "prefix")
        |> Option.bind tryAsString
        |> Option.defaultValue ""

    let tryGetNameAndCodeProperty (prefix:string) (json:JsonParser.Json) =
        match json with
        | JsonParser.Json.Object (properties) ->

                        // let name = prefix + jo.Value<string>("name")
                        // let code = System.Char.ConvertFromUtf32 (jo.Value<int>("code")) |> string
                        // Some (name, code)
            let name =
                properties
                |> List.tryPick (fun (name, value) ->
                    if name = "name" then
                        value
                        |> tryAsString
                        |> Option.map (fun str -> prefix + str)
                    else None )
            let code =
                properties
                |> List.tryPick (fun (name, value) ->
                    if name = "code" then
                        value
                        |> tryAsNumber
                    else None )

            match name, code with
            | Some name, Some code ->
                Some (name, System.Char.ConvertFromUtf32 ( code |> System.Convert.ToInt32) |> string)
            | _ -> None

        | _ -> None

    let getIconProperties prefix (json:JsonParser.Json) =
        json
        |> tryFindObjectProperty "icons"
        |> Option.bind tryAsArray
        |> Option.map (fun icons ->

            icons
            |> List.choose (fun icon ->
                icon
                |> tryFindObjectProperty "properties"
                |> Option.bind (tryGetNameAndCodeProperty prefix)
            )
            // tryFindObjectProperty "prefix"
        )
        |> Option.defaultValue []
        //json.SelectToken("preferences.fontPref.prefix") |> Option.ofObj |> Option.map (fun t -> t.Value<string>() )  |> Option.defaultValue ""

    let buildTypes typeName (pVals:obj[]) =
        match pVals with
        | [| :? string as arg|] ->
                let content =
                    if arg.StartsWith("{") || arg.StartsWith("[") then arg
                    else if Regex.IsMatch(arg, "^https?://") then
                        async {
                            let! (status, res) = Http.get arg
                            if status <> 200 then
                                return failwithf "URL %s returned %i status code" arg status
                            return res
                        } |> Async.RunSynchronously
                    else
                        let filepath =
                            if System.IO.Path.IsPathRooted arg then
                                arg
                            else
                                System.IO.Path.GetFullPath(System.IO.Path.Combine(config.ResolutionFolder, arg))

                        let weakRef = System.WeakReference<_>(this)

                        let _  =
                            watcherSubscriptions.GetOrAdd(
                                typeName,
                                fun _ -> IO.watchForChanges filepath (typeName + (this.GetHashCode() |> string) , fun () ->
                                    match weakRef.TryGetTarget() with
                                    | true, t -> t.Invalidate()
                                    | _ -> ()))

                        System.IO.File.ReadAllText(filepath,System.Text.Encoding.UTF8)


                let json = JsonParser.parse content
                if json.IsNone then failwith "invalid json"



                let prefix = json.Value |> getFontPrefix

                let chars = json.Value |> getIconProperties prefix

                convertChars asm ns typeName chars

        | _ -> failwith "unexpected parameter values"

    do this.Disposing.Add(fun _ -> watcherSubscriptions |> Seq.iter ( fun kv -> kv.Value.Dispose()) )

    do generator.DefineStaticParameters(
        parameters = staticParams,
        instantiationFunction = buildTypes
        )

    do this.AddNamespace(ns, [generator])

[<assembly:TypeProviderAssembly>]
do ()