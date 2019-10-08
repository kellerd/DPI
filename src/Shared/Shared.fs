namespace Shared
open System
type SessionId = int
type SessionName = string

type Registrant = {Name:string}
type RegistrationStatus = 
    | Reserved
    | Waitlist
type Pass = { PassNumber:int }
type Registration = {Registrant: Registrant; Date: string; Status: RegistrationStatus; Pass: Pass option }
type SessionDetails = {Name:SessionName;NameFr:SessionName; Date:string; EndDate:string;Gap:float}
[<CLIMutable>]
type Session = {Id:int; Details: SessionDetails; Registrations: Registration list; FreePasses: Pass list; }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type SecurityToken = SecurityToken of string

type LoginInfo = { Username: Registrant; Pin: string }


// possible errors when logging in
type LoginError = 
    | PinIncorrect
    | CouldNotFindUser

// a request with a token
type SecureRequest<'t> = { Token : SecurityToken; Content : 't }

// possible authentication/authorization errors     
type AuthenticationError = 
   | TokenInvalid
   | UserDoesNotHaveAccess

type Errors =
    | UpdateError
    | MaxReserved
    | NotFound
    | AlreadyReserved

type CancelResult = 
    | Cancelled 
    | CancelError
    | AuthError

type ReserveResult =
    | Recorded of (RegistrationStatus *Pass option)
    | ReserveError of Errors
    | AuthError

type AddResult =
    | Added
    | AddError
/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ISessionApi =
    { 
        login : LoginInfo -> Async<Result<SecurityToken, LoginError>>
        getSessions : unit -> Async<List<Session>>
        getProfile : SecureRequest<SecurityToken> -> Async<Result<Registrant, AuthenticationError>> 
        getSession : SessionId -> Async<Option<Session>>
        reserve : SecureRequest<SessionId> -> Async<Result<ReserveResult, AuthenticationError>>
        cancel : SecureRequest<SessionId> -> Async<Result<CancelResult, AuthenticationError>>
        addSession : SecureRequest<Session> -> Async<Result<Option<Session>, AuthenticationError>> 
        resetPassword : LoginInfo -> Async<Result<SecurityToken, LoginError>>
        fixDate : string * string * SessionId -> Async<Option<Session>>
        addPasses : int * SessionId -> Async<Option<Session>>
        delete : SessionId -> Async<unit>
    }
    
module Session =
    let dateFormat = "yyyy-MM-ddTHH:mm:ss.000-04:00"
    let defaultPasses = List.init 20 (fun i -> {PassNumber = i + 1} )
    let defaultSession    = {Id = 0;
                             Details = {
                                Name = ""; 
                                NameFr = ""; 
                                Gap = 30.; 
                                Date = System.DateTime.Now.ToString(dateFormat); 
                                EndDate=System.DateTime.Now.AddHours(2.).ToString(dateFormat) }
                             Registrations = []
                             FreePasses = defaultPasses  } 
module Map = 
    let update key update def map  =
        let item =
            match Map.tryFind key map with
            | Some i -> i |> update
            | None -> def |> update
        Map.add key item map 
            
module List =
    let update pred update list =
        List.map (fun i -> if pred i then update i else i ) list

module Array = 
    let chunkIt chunkSize (array:'T[]) =
        if chunkSize <= 0 then failwith "Invalid chunkSize"
        let len = array.Length
        if len = 0 then
            [||]
        else if chunkSize > len then
            [| array |]
        else
            let chunkCount = (len - 1) / chunkSize + 1
            let res = Array.zeroCreate chunkCount : 'T[][]
            for i = 0 to len / chunkSize - 1 do
                res.[i] <- Array.sub array (i * chunkSize) chunkSize
            if len % chunkSize <> 0 then
                res.[chunkCount - 1] <- Array.sub array ((chunkCount - 1) * chunkSize) (len % chunkSize) 
            res        
// F# 
// 76+253 = 329  Server
// C# 3157+1686+462 = 5305
// 113 shared
// 634+94+549 = 1277 Client
// JS 2000~13000