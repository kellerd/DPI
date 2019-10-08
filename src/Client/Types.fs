module Types 
open Shared

open Shared
open Fulma
open System
open Fable.PowerPack.Date.Local
open Fable.Import.React

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type RegistrationSession = 
    {   Session : Session; 
        Loading : bool; 
        Message : string option; 
        MessageClass: Message.Option list }

type InfoSession = 
    {
        Details : SessionDetails
        Description : ReactElement * ReactElement
    }

type ClientSession = CanRegister of RegistrationSession | InfoOnly of InfoSession
type PassNumber = int                       
type Model = { 
               ForgetPin : bool
               FocusTarget : float option
               AddingLoading: bool
               Token: SecurityToken option
               CurrentUser: Registrant option; 
               Sessions: (Map<int, ClientSession>) option;
               UserPasses: (PassNumber * SessionDetails) list
               NewSessionErrorText :  Result<string,string>
               NewSessionErrorTextFr : Result<string,string>
               NewSessionErrorDate : Result<string,string>
               NewSessionErrorEndTime : Result<string,string>
               NewSessionErrorGap : Result<float,string>
               NewSessionErrorTime : Result<string,string>
               NewSession: SessionName * SessionName * string * string * string * string
               GlobalError: string option
               Local: Localization
               LoginName : string
               LoginError : Result<string,string>
               Pin : string
               PinError : Result<string,string>
               Resources: Map<(string*Localization),string>  }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type SessionUpdate = | Reg of ReserveResult | Can of CancelResult 
type PIN = int16
type LoginResult = 
    | LoginError of LoginError
    | LoginException of exn
    | LoginSuccess of SecurityToken
type Msg =
| SessionsLoaded of Result<Session list, exn>
| Reserve of SessionId
| CancelReservation of SessionId
| Focus of float
| StoreFocus of float
| Refresh of SessionId
| SessionUpdated of SessionId * Result<SessionUpdate, exn>
| SessionLoaded of SessionId option * Result<Session option, exn>
| AddSession of SessionName * SessionName * string * string * float
| ValidateNewSession 
| DescriptionChanged of SessionName 
| DescriptionChangedFr of SessionName 
| DateChanged of string
| PinChanged of string
| NameChanged of string 
| TimeChanged of string
| GapChanged of string
| CancelResetPin
| EndTimeChanged of string
| SetHtmlLanguage of string
| SwitchLanguage
| LoggedIn of LoginResult
| Login of LoginInfo
| Logout
| TryGetToken
| LoadedProfile of Result<Registrant, AuthenticationError >
| ValidateLogin
| AskToResetPin
| ForgetMyPin of LoginInfo

let getResource model key =
    match Map.tryFind (key,model.Local) model.Resources with
    | Some s -> s
    | None -> failwith (sprintf "Couldn't find key %s in resources" key)
