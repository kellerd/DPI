module WebApp
open System
open Shared
open System.Security.Cryptography
open System.IO
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions
open Microsoft.AspNetCore.Http
open System.Text
open System.Globalization
[<CLIMutable>]
type LoginHash = {Id: int; User:Registrant; Hash : string; Salt : string; Token: string}

let maxReserved session = 
    session.FreePasses.Length = 0
let alreadyReserved session (registrant:Registrant) = 
    session.Registrations 
    |> List.exists (fun r -> String.Compare ( r.Registrant.Name, registrant.Name, true) = 0)

let nextPass session = 
    match session.FreePasses with 
    | [] ->         Waitlist, None, [] 
    | head::tail -> Reserved, Some head,tail

let reserveSession registrant session = 
    let (status, pass,freePasses) = nextPass session
    let registration = { Registrant = registrant; Status = status; Pass = pass; Date= DateTime.Now.ToString(Session.dateFormat) }
    let registrations = session.Registrations @ [ registration ] |> List.sortBy (fun r -> r.Pass)
    pass, status, { session with FreePasses = freePasses; Registrations = registrations }

let CancelReservationSession (registrant:Registrant) session =
    let (passes,registrations) =
        List.foldBack (fun item (passes,registrations) ->
                                    match item.Pass, ( String.Compare ( item.Registrant.Name, registrant.Name, true) = 0) with 
                                    | Some pass, true -> pass :: passes, registrations
                                    | None, true -> passes, registrations
                                    | _, false -> passes, item :: registrations)  session.Registrations (session.FreePasses, [])
    {session with Registrations = registrations; FreePasses = passes}  

let createUsing (db: unit->LiteDatabase) : ISessionApi = 

    let sessions (db:LiteDatabase) = db.GetCollection<Session>("sessions")
    let users  (db:LiteDatabase) = db.GetCollection<LoginHash>("users")

    let addSession session = 
        use database = db()
        let sessions = sessions database
        let result = sessions.Insert(document= session)
        result |> sessions.TryFindById

    let getSessions () = async {
        use database = db()
        let sessions = sessions database
        return 
            sessions.FindAll () 
            |> Seq.sortBy (fun s -> s.Details.Date)
            |> Seq.toList
    }

    let getSession (sessionId : int) = async {
        use database = db()
        let sessions = sessions database
        let id = BsonValue(sessionId)
        return sessions.TryFindById id
    }

    let  getMd5Hash (md5:MD5) (input:string) = 
        
        let sBuilder : StringBuilder =
            input
            |> Encoding.UTF8.GetBytes
            |> md5.ComputeHash
            |> Array.fold (fun (sBuilder) s -> sBuilder.Append(s.ToString("x2")))  (StringBuilder())
        sBuilder.ToString();

    let login (loginInfo:LoginInfo) = async {
        use database = db()
        let users = users database
        let user = 
            users.FindAll() 
                |> Seq.tryFind ( fun u -> String.Compare(u.User.Name, loginInfo.Username.Name,true) = 0 )
        use md5 = MD5.Create()
        let loginResult =
            match user with 
            | Some u -> 
                let hsh = getMd5Hash md5 (loginInfo.Pin + u.Salt) 
                if String.Compare(hsh, u.Hash, true) = 0 then Ok (SecurityToken u.Token)
                else Error PinIncorrect   
            | None ->
                let salt = getMd5Hash md5 (System.Guid.NewGuid().ToString())
                let hsh = getMd5Hash md5 (loginInfo.Pin + salt) 
                let newUser =  {Id = 0; User = loginInfo.Username; Hash = hsh; Salt = salt ; Token = System.Guid.NewGuid().ToString()}
                let result = users.Insert(document= newUser)
                match result |> users.TryFindById with
                | Some u -> Ok (SecurityToken u.Token)
                | None -> Error CouldNotFindUser
        return loginResult    
    }


    let resetPassword (loginInfo:LoginInfo) = async {
        
        use database = db()
        let users = users database
        let user = 
            users.FindAll() 
                |> Seq.tryFind ( fun u -> String.Compare(u.User.Name, loginInfo.Username.Name,true) = 0 )
        match user with 
        | Some user -> 
            users.Delete(id=BsonValue(user.Id)) |> ignore
            return! login loginInfo 
        | None -> 
        return Error CouldNotFindUser
    }

    let getProfile { Token = SecurityToken token; Content = _ }  = async {
        
        use database = db()
        let users = users database
        let user = 
            users.tryFindOne <@ fun u -> u.Token = token @> 
            |> Option.map (fun u -> u.User)
        return  
            match user with 
            | Some user -> Ok(user)
            | None -> Error AuthenticationError.TokenInvalid    
    }

    let secureCall f ({ Token = SecurityToken token; Content = x } as t)  = async {
        
        let! registrant = getProfile t
        match registrant with
        | Ok y -> 
            let result = f x y
            return result
        | Error e -> return Error e
    }

    let onlyAdmins f x (registrant:Registrant) = 
        match registrant with 
        | {Name = "Danny Keller"}
        | {Name = "Marcel Paquet"} -> Ok (f x) 
        | _ -> Error AuthenticationError.UserDoesNotHaveAccess


    let reserve (sessionId:int) registrant = 
        use database = db()
        let sessions = sessions database
        let id = BsonValue(sessionId)
        let session = sessions.TryFindById id
        session
        |> Option.map (fun session -> 
            if maxReserved session then ReserveError MaxReserved
            else if alreadyReserved session registrant then ReserveError AlreadyReserved
            else  
                let (pass, state, newSession) = reserveSession registrant session
                let updateResult = sessions.Update newSession
                if updateResult then Recorded (state, pass) 
                else ReserveError UpdateError) 
        |> Option.defaultValue (ReserveError NotFound)

    let cancel (sessionId:int) registrant = 
        use database = db()
        let sessions = sessions database
        let id = BsonValue(sessionId)
        let session = sessions.TryFindById id
        session
        |> Option.map (fun session -> 
            let updateResult = 
                CancelReservationSession registrant session
                |> sessions.Update
            if updateResult then Cancelled
            else CancelError) 
        |> Option.defaultValue (CancelError)
    let fixDate (startDate,endDate,sessionId) = async {
        use database = db()
        let sessions = sessions database
        let id = BsonValue(sessionId:int)
        let session = sessions.TryFindById id
        session
        |> Option.iter (fun session -> 
              { session with Details = { session.Details with Date = startDate; EndDate = endDate } }
              |> sessions.Update |> ignore
        )
        return sessions.TryFindById id
    }

    let addPasses (totalPasses,sessionId) = async {
        use database = db()
        let sessions = sessions database
        let id = BsonValue(sessionId:int)
        let session = sessions.TryFindById id
        session
        |> Option.iter (fun session -> 
              let passesLength = session.FreePasses.Length
              let registrations = session.Registrations.Length
              let newPasses = 
                let difference = totalPasses - registrations - passesLength
                if difference > 0 then
                    session.FreePasses @ List.init difference (fun i -> {PassNumber = i + 1 + registrations + passesLength })
                else session.FreePasses |> List.truncate (totalPasses - registrations)
              { session with FreePasses = newPasses }
              |> sessions.Update |> ignore
        )
        return sessions.TryFindById id
    }

    let delete = fun sessionId -> async {
        use database = db()
        let sessions = sessions database          
        let id = BsonValue(sessionId:int)
        do sessions.Delete(id)  |> ignore
    }

    let app : ISessionApi = {
        login = login
        getSessions = getSessions 
        getSession = getSession 
        // getUsers = getUsers 
        resetPassword = resetPassword  
        getProfile = getProfile                     
        reserve = secureCall (fun a r -> reserve a r |> Ok)
        cancel = secureCall (fun a r -> cancel a r |> Ok) 
        addSession = secureCall (onlyAdmins addSession)
        fixDate = fixDate
        delete = delete
        addPasses = addPasses
    }

    app 

let createUsingInMemoryStorage() : ISessionApi = 
    // In memory collection
    let memoryStream = new MemoryStream()
    let bsonMapper = FSharpBsonMapper()
    let inMemoryDatabase () = new LiteDatabase(memoryStream, bsonMapper)
    createUsing inMemoryDatabase

let createUsingDbStorage (dbName:string) : ISessionApi = 
    // In memory collection
    let bsonMapper = FSharpBsonMapper()
    let inMemoryDatabase () = new LiteDatabase(dbName, bsonMapper)
    createUsing inMemoryDatabase

let seedIntitialData (isession: ISessionApi) = 
    let sessions = 
        [
            {Session.defaultSession with Details ={ Date="2019-05-22T08:00:00.000-04:00" ; Gap = 30.; EndDate = "2019-05-22T09:45:00.000-04:00" ;  Name="Registration / Keynote: K1";           NameFr="Registration / Keynote: K1"}}       |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-22T10:15:00.000-04:00";  Gap = 15.; EndDate = "2019-05-22T11:15:00.000-04:00";   Name="Sessions: W1/S1/S2/S3/M1" ;            NameFr="Sessions: W1/S1/S2/S3/M1"}}     |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-22T11:30:00.000-04:00";  Gap = 60.; EndDate = "2019-05-22T12:30:00.000-04:00";   Name="Sessions: S4/S5/S6/M2" ;               NameFr="Sessions: S4/S5/S6/M2"}}        |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-22T13:30:00.000-04:00" ; Gap = 30.; EndDate = "2019-05-22T14:30:00.000-04:00" ;  Name="Sessions: S7/S8/S9/S10/M3" ;           NameFr="Sessions: S7/S8/S9/S10/M3"}}        |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-22T15:00:00.000-04:00" ; Gap = 0.;  EndDate = "2019-05-22T16:00:00.000-04:00" ;  Name="Keynote: K2" ;                        NameFr="Keynote: K2"}}                   |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-23T08:00:00.000-04:00" ; Gap = 30.; EndDate = "2019-05-23T09:45:00.000-04:00" ;  Name="Registration / Keynote: K3" ;          NameFr="Registration / Keynote: K3"}}               |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-23T10:15:00.000-04:00";  Gap = 15.; EndDate = "2019-05-23T11:15:00.000-04:00";   Name="Sessions: S11/S12/S13/S14" ;           NameFr="Sessions: S11/S12/S13/S14"}}                |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-23T11:30:00.000-04:00";  Gap = 60.; EndDate = "2019-05-23T12:30:00.000-04:00";   Name="Sessions: S15/S16/S17/S18" ;           NameFr="Sessions: S15/S16/S17/S18"}}                |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-23T13:30:00.000-04:00" ; Gap = 30.; EndDate = "2019-05-23T14:30:00.000-04:00" ;  Name="Sessions: S19/S20/S21/S22" ;           NameFr="Sessions: S19/S20/S21/S22"}}                |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-23T15:00:00.000-04:00" ; Gap = 0.;  EndDate = "2019-05-23T16:00:00.000-04:00" ;  Name="Keynote: K4" ;                        NameFr="Keynote: K4"}}               |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-24T08:00:00.000-04:00" ; Gap = 30.; EndDate = "2019-05-24T09:45:00.000-04:00" ;  Name="Registration / Keynote: K5" ;           NameFr="Registration / Keynote: K5"}}              |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-24T10:15:00.000-04:00";  Gap = 15.; EndDate = "2019-05-24T11:15:00.000-04:00";   Name="Sessions: S23/S24/S25/W2" ;            NameFr="Sessions: S23/S24/S25/W2"}}             |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-24T11:30:00.000-04:00";  Gap = 60.; EndDate = "2019-05-24T12:30:00.000-04:00";   Name="Sessions: S26/S27/S28/W2" ;            NameFr="Sessions: S26/S27/S28/W2"}}             |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
            {Session.defaultSession with Details ={ Date="2019-05-24T13:30:00.000-04:00" ; Gap = 0.;  EndDate = "2019-05-24T14:30:00.000-04:00" ;  Name="Keynote: K6" ;                        NameFr="Keynote: K6"}}               |> reserveSession {Name = "Marcel Paquet"} |> fun (_,_,s) -> s
        ]

    let login = isession.login {Username = {Name = "Danny Keller"}
                                Pin = "000"}
                |> Async.RunSynchronously
    match login with 
    | Ok token ->  
        sessions
        |> List.map (fun x -> isession.addSession {Token = token; Content = x})
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
    | Error(errorValue) -> failwith (sprintf "Login error %A" errorValue)
