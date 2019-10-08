open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Giraffe
open Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = "public_path" |> tryGetEnv |> Option.defaultValue  "../Client/public" |> Path.GetFullPath
let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

type DBEnvironment = | InMemory | File of string

let db = 
    match "DB" |> tryGetEnv with
    | Some "InMemory" -> InMemory
    | Some x -> File x
    | None -> File "simple.db"

let errorHandler (ex : Exception) (logger:ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse
    >=> ServerErrors.INTERNAL_ERROR ex.Message

let webApi =
    
    let sessionApi = 
        match db with
        | InMemory -> WebApp.createUsingInMemoryStorage()
        | File x when x.EndsWith(".db") -> WebApp.createUsingDbStorage x
        | File x -> WebApp.createUsingDbStorage (x + ".db")

    let sessions = sessionApi.getSessions () |> Async.RunSynchronously
    if sessions.Length = 0 then
        WebApp.seedIntitialData sessionApi

    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue sessionApi
    |> Remoting.withErrorHandler (fun ex routeInfo -> Propagate ex.Message) 
    |> Remoting.buildHttpHandler
    
let mainApp =  
    setHttpHeader "Cache-Control" "max-age=5" 
    >=> webApi 

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
        .UseStaticFiles()
        .UseGiraffeErrorHandler(errorHandler)
        .UseGiraffe mainApp

let configureServices (services : IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services 
    |> fun s -> s.AddGiraffe() 
    |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()