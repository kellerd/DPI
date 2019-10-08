module State
open Types
open Shared
open Elmish
open Fulma
open System
open Shared
open Fable.PowerPack.Date.Local
open Elmish.Browser.Navigation
open Fable.PowerPack
open Fable.Helpers.React
open Fable.Import.React
module Server =
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api =
        Remoting.createApi()
        |> Remoting.withCustomHeader ["Cache-Control", "maxage=5"]
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.buildProxy<ISessionApi>

let initialPasses = Server.api.getSessions



let initClientSession = 
    { Message = None; 
      Loading = false;
      Session = Session.defaultSession
      MessageClass = [] } |> Types.CanRegister 
// defines the initial state and initial command (= side-effect) of the application
let init () : Model=
    let add key (e,f) =
        Map.add (key,englishUK) e
        >> Map.add (key,french) f
    let resources = 
        [
            "Couldn'tFindSession", ("Couldn't find session in the database it may have been removed, please refresh screen to try again.", "Impossible de trouver la session dans la base de données, elle a peut-être été supprimée, veuillez régénérer l'écran pour réessayer.")
            "MaxPasses", ("The maximum number of passes have already been allocated for this session.", "Le nombre maximum de laissez-passer a déjà été alloué pour cette session.")
            "AlreadyReserved", ("You're already reserved to this session.", "Vous avez déjà réservé cette session.")
            "NoDelete", ("I couldn't delete your reservation, please contact Marcel Paquet and Dan Keller using the DL OTT AFCCG AMS IM distribution list to get this updated.", "Je ne pouvais pas supprimer votre réservation, veuillez contacter Marcel Paquet et Dan Keller en utilisant la liste de distribution DL OTT AFCCG AMS IM pour obtenir cette mise à jour.")
            "NoAdd", ("I couldn't add your reservation, please contact Marcel Paquet and Dan Keller using the DL OTT AFCCG AMS IM distribution list to get this updated.", "Je ne pouvais pas ajouter votre réservation, veuillez contacter Marcel Paquet et Dan Keller en utilisant la liste de distribution DL OTT AFCCG AMS IM pour obtenir cette mise à jour.")
            "NoLoad", ("Couldn't load data from the database, please refresh screen to try again.", "Impossible de charger les données de la base de données, veuillez régénérer l'écran pour réessayer.")
            "NoSession", ("Couldn't find session in the database it may have been removed, please refresh the screen to try again.", "Impossible de trouver la session dans la base de données, elle a peut-être été supprimée, veuillez régénérer l'écran pour réessayer.")
            "DateRequired", ("Date is required.", "La date est requise.")
            "DateWrongFormat", ("The date is in the wrong format. Please use the YYYY-MM-DD date format.", "La date est dans le mauvais format. Veuillez utiliser le format de date AAAA-MM-JJ.")
            "TimeRequired", ("Time is required.", "Le temps est requis.")
            "TimeWrongFormat", ("Time is in the wrong format. The expected time format is HH:MM.", "L'heure est dans le mauvais format. Le format attendu est le HH: MM.")
            "EnglishDescRequired", ("English title is required.", "Le titre en anglais est requis.")
            "FrenchDescRequired", ("French title is required.", "Le titre en français est requis.")
            "Couldn'tAddSession", ("There is a problem adding the session to the database.", "Il y a un problème lors de l'ajout de la session à la base de données.")
            "Hello", ("Hello, ", "Allo, ")
            "CancelReservation", ("Cancel reservation", "Annuler la réservation")
            "CancelReservationAlt", ("Cancel reservation for", "Annuler la réservation pour")
            "CountAlt", ("Number of reserved passes", "Nombre de laissez-passer réservés")
            "Reserve", ("Reserve", "Réserver")
            "Refresh", ("Reload session information", "Recharger les informations de la session")
            "ReserveAlt", ("Reserve a pass for", "Réserver un laissez-passer pour")
            "Cancel", ("Cancel", "Annuler")
            "Pass", ("Pass", "Laissez-passer")
            "Name", ("Name", "Nom")
            "Reservations", ("Reservations", "Des réservation")
            "SessionFull", ("This session is currently full. Please check back for further updates.", "Cette session est actuellement pleine. Veuillez vérifier à nouveau pour d'autres mises à jour.")
            "Loading", ("Loading...", "Chargement...")
            "NotMe", ("<Not Me>", "<Pas moi>")
            "Logo", ("Logo", "Logo")
            "Home", ("Home", "Accueil")
            "AlternateLanguage", ("Français", "English")
            "Examples", ("Examples", "Exemples")
            "Docs", ("Documentation", "Documentation")
            "ViewSource", ("View Source", "Voir la source")
            "YourPasses", ("Your Sessions", "Vos sessions")
            "PleaseSessionDesc", ("Please insert an english session title.", "Veuillez insérer un titre de session en anglais.")
            "PleaseSessionDescFr", ("Please insert a french session title.", "Veuillez insérer un titre de session en français.")
            "SessionDescription", ("English Session Title", "Titre de la session anglaise")
            "SessionDescriptionFr", ("French Session Title", "Titre de la session française")
            "SessionDate", ("Session Date", "Date de la session")
            "SessionEndTime", ("Session End Time", "Temps de fin de session")
            "SessionTime", ("Session Time", "Temps de la session")
            "AddSession", ("Add Session", "Ajouter une session")
            "EnterName", ("Enter full name (First name Last name)", "Entrez le nom complet (Prénom Nom de famille)")
            "EnterPin", ("Enter a 3 digit PIN", "Entrez un code NIP à 3 chiffres")
            "NameRequired", ("Your full name is required (First Name Last Name).", "Votre nom complet est requis (Prénom Nom de famille).")
            "PINRequired", ("Give a short PIN to lock down your records.", "Donnez un code NIP court pour verrouiller vos enregistrements.")
            "Name", ("Name", "Prénom")
            "PIN", ("PIN", "NIP")
            "Introduce", ("Hello, please introduce yourself.", "Allo, veuillez vous présenter.")
            "Login", ("Go!", "Aller!")
            "Reserved", ("Reserved", "Réservé")
            "Registration Msg", ("Please enter your name and a 3 digit PIN so you can lock your reservations.", "Veuillez entrer votre nom et un code NIP à 3 chiffres pour pouvoir verrouiller vos réservations.")
            "Waitlist", ("Waitlist", "Liste d'attente")
            "Add a session", ("Add a session", "Ajouter une session")
            "DPI Registration", ("DPI - TC Pass Reservation", "Réservation de Laissez-passer TC - DPI")
            "PinIncorrect", ("This is probably not the same PIN that you've used before.", "Ce n'est probablement pas le même code NIP que vous avez utilisé auparavant.")
            "CouldNotFindUser", ("Could not reserve you, contact Dan Keller or Marcel Paquet using the DL OTT AFCCG AMS IM distribution list.", "Impossible de vous réserver, contactez Dan Keller ou Marcel Paquet en utilisant la liste de distribution DL OTT AFCCG AMS IM.")
            "Token validation Error", ("Could not read previous credentials, please login again.", "Impossible de lire les informations d'identification précédentes, veuillez vous reconnecter.")
            "User not authenticated", ("User not authenticated", "Utilisateur non authentifié")
            "Loading", ("Loading...", "Chargement...")
            "Note", ("NOTE – Passes that are being shared are to be returned to DPI registration desk at the Shaw Centre after each block of session(s), before returning to the office.", "Les laissez-passer partagés doivent être retournés au bureau d'inscription DPI du Centre Shaw après chaque bloc de session(s), avant de retourner au bureau.") 
            "Schedule", ("Schedule of Events", "Horaire des événements")
            "SessionInfo", ("Session Information", "Information de la session")
            "SessionInfoAlt", ("Session Information for", "Information de la session pour")
            "ResetPin", ("Reset PIN", "Réinitialiser le code NIP")
            "NewPin", ("New PIN", "Nouveau NIP")
            "BreakTimeHowLong", ("How long of a break untill the next session", "Combien de temps pour la pause jusqu'à la prochaine session ?")
            "BreakTime", ("Break Time", "Pause")
            "SessionInfoLink", ("https://dpi-canada.com/program/", "https://dpi-canada.com/fr/programme/")
            "BreakTimeRequired", ("Break Time is required", "Le temps de pause est requis.")
            "BreakTimeFormat", ("Could not read the break time as a number", "Impossible de lire l'heure de la pause sous la forme d'un nombre")
            "ChangeYourPin", ("Change your PIN to something else", "Changez votre NIP pour quelque chose d'autre.")
            "ScheduleLink", ("https://dpi-canada.com/wp-content/uploads/2018/12/pdw2019schedule.pdf", "https://dpi-canada.com/wp-content/uploads/2018/12/pdw2019schedule.pdf")
            "ol1a", ("Please review the ", 
                     "S'il vous plaît consulter le ")
            "ol1b", ("https://dpi-canada.com/wp-content/uploads/2018/12/pdw2019schedule.pdf", "https://dpi-canada.com/wp-content/uploads/2018/12/pdw2019schedule.pdf")
            "ol1c", ("schedule of events", "calendrier des événements")
            "ol1d", (" and indicate the timeslot you wish to attend.", " et indiquer le créneau horaire de la session que vous souhaitez assister.")
            "ol2a", ("Please don't forget to become a DPI affiliate by simply completing and submitting the ", "N'oubliez pas de devenir un affilié de DPI en remplissant et en soumettant simplement le ")
            "ol2b", ("https://dpi-canada.com/get-involved/affiliate-application-form/", "https://dpi-canada.com/fr/impliquez-vous/formulaire-daffiliation/")
            "ol2c", ("Affiliate Application Form", "formulaire de demande d'affiliation")
            "ol2d", (".", ".")
            "ol3", ("Please don't forget to get your manager’s approval.", "S'il vous plaît ne pas oublier d’obtenir l’autorisation de votre gestionnaire.")

            

        ] |> List.fold (fun map (k,v) -> add k v map) Map.empty<_,_>
        
    let initialModel = { 
                         ForgetPin = false
                         FocusTarget = None
                         AddingLoading = false
                         CurrentUser = None //Some {Name = "Danny Keller"}
                         Token = None
                         Sessions = None
                         UserPasses = []
                         NewSession = "", "", System.DateTime.Now.ToString("yyyy-MM-dd"), System.DateTime.Now.ToString("HH") + ":00", System.DateTime.Now.AddHours(1.).ToString("HH") + ":00", "0"
                         GlobalError =  None
                         NewSessionErrorText = Ok ""
                         NewSessionErrorGap = Ok 0.
                         NewSessionErrorTextFr = Ok ""
                         NewSessionErrorTime = Ok ""
                         NewSessionErrorDate = Ok ""
                         NewSessionErrorEndTime = Ok ""
                         Pin = ""
                         PinError = Ok ""
                         LoginName = ""
                         LoginError = Ok ""
                         Local = englishUK
                         Resources = resources }

    initialModel
let samePersonTest (r:Registrant) u = 
    Some true  = Option.map (fun (user : Registrant ) -> String.Compare(user.Name, r.Name, true) = 0) u
let updateSessionsLoad msg currentModel = 
    match msg with 
    | Error _-> 
        let restart () = async {
            let! retryWait = Async.Sleep 1500
            return! initialPasses()
        }
        let restartCmd = 
            Cmd.ofAsync
                restart 
                ()
                (Ok >> SessionsLoaded)
                (Error >> SessionsLoaded)
        currentModel, restartCmd
    | Ok initialSessions ->
        let userPasses, sessionPasses = 
            initialSessions 
            |> List.map (fun s -> 
                let user = s.Registrations 
                           |> List.choose (fun r -> r.Pass |> Option.map (fun p -> p.PassNumber,r.Registrant,s.Details)) 
                           |> List.filter (fun (_,r,_) -> samePersonTest r currentModel.CurrentUser)
                           |> List.map (fun (i,_,details) -> i,details)
                let clientSession = {Session= s; Loading = false; Message = None; MessageClass=[]}
                user,clientSession
            )
            |> List.unzip
        let boxcarSocial = 
            { 
              Details = 
                { Date="2019-05-22T16:00:00.000-04:00" ;
                  Gap = 0.; 
                  EndDate = "2019-05-22T18:00:00.000-04:00" ;  Name="SOCIAL EVENT – EVERYONE IS INVITED!"
                  NameFr="SOCIAL EVENT – EVERYONE IS INVITED!"}
              Description = 
                  article [] [
                    p [] [str "All DPI affiliates are invited!"]

                    p [] [str "Please "; a [Props.Class "tag is-medium"; Props.Target "_blank"; Props.Href  "https://dpi-canada.com/events/event-index/"] [str "register for the social event"]; str " on the DPI web site prior to the event."] ],
                  article [] [
                      p [] [str "Tous les affiliés de DPI sont invités!"]

                      p [] [str "Veuillez vous "; a [Props.Class "tag is-medium"; Props.Target "_blank"; Props.Href "https://dpi-canada.com/fr/evenements/calendar-of-events/"] [str "inscrire à l’événement social"]; str " sur le site Web DPI avant l’événement."] ]
              
            } |> InfoOnly
        let nextModel = { currentModel with Sessions = Some ((-1, boxcarSocial) :: (sessionPasses |> List.map(fun s -> s.Session.Id, CanRegister s)) |> Map.ofList)
                                            UserPasses = userPasses |> List.collect id }
        nextModel, Cmd.none
let updateWithLoading sessionId =
    Map.update sessionId (
        function 
        | InfoOnly p -> InfoOnly p
        | CanRegister p -> 
          CanRegister 
              { p with 
                    Message = None
                    Loading = true
                    MessageClass = []  } ) initClientSession
    |> Option.map   
let updateWithSuccess sessionId sessionUpdate =
    Map.update sessionId (
        function 
        | InfoOnly p -> InfoOnly p
        | CanRegister p -> 
          CanRegister 
           { p with 
                 Message = None
                 Loading = false
                 MessageClass = []
                 Session = sessionUpdate p.Session  }
         ) initClientSession
    |> Option.map

let updateWithErrors sessionId message = 
    Map.update sessionId (
        function 
        | InfoOnly p -> InfoOnly p
        | CanRegister p -> 
          CanRegister 
            { p with 
                Message = Some message
                Loading = false
                MessageClass = [Message.Color IsWarning] } ) initClientSession
    |> Option.map
let sessionUpdated sessionId msg currentModel = 
    let newModel = 
        match msg with     
        | Ok(Can CancelResult.AuthError) ->
            { currentModel with GlobalError = Some "Could not be authenticated to cancel" } 
        | Ok(Reg AuthError) ->
            { currentModel with GlobalError = Some "Could not be authenticated to reserve" } 
        | Ok(Reg(ReserveError NotFound)) -> 
            let sessions = updateWithErrors sessionId  (getResource currentModel "Couldn'tFindSession") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            newModel
        | Ok(Reg(ReserveError MaxReserved)) ->
            let sessions = updateWithErrors sessionId (getResource currentModel "MaxPasses") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            newModel
        | Ok(Reg(ReserveError AlreadyReserved)) ->
            let sessions = updateWithErrors sessionId (getResource currentModel "AlreadyReserved") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            newModel
        | Ok(Can(CancelError)) ->
            let sessions = updateWithErrors sessionId  (getResource currentModel "NoDelete") currentModel.Sessions
            let newModel = {currentModel with Sessions = sessions }
            newModel
        | Ok(Reg(ReserveError(UpdateError))) ->
            let sessions = updateWithErrors sessionId  (getResource currentModel "NoAdd") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            newModel
        | Ok(Can(Cancelled)) -> 
            let sessions = 
                updateWithSuccess sessionId (fun session -> 
                    { session with 
                        Registrations = 
                            session.Registrations 
                            |> List.filter (fun r -> samePersonTest r.Registrant currentModel.CurrentUser |> not) } ) currentModel.Sessions
            let newModel = {currentModel with Sessions = sessions }            
            newModel
        | Ok(Reg(Recorded(status,pass))) -> 
            match currentModel.CurrentUser with 
            | Some user -> 
                let newRegistration = {Registrant= user; Date = System.DateTime.Now.ToString(Session.dateFormat); Status = status; Pass = pass }
                let sessions = 
                    updateWithSuccess sessionId (fun session -> 
                        let registrations = 
                            List.append session.Registrations [ newRegistration ]
                        { session with Registrations = registrations } ) currentModel.Sessions
                let newModel = {currentModel with Sessions = sessions }            
                newModel
            | None -> currentModel
        | Error(exn)->    
            raise (exn)
            currentModel  
    newModel, Cmd.none  
let sessionLoaded sessionId msg currentModel = 
    let newModel = 
        match msg with 
        | Ok(Some session) ->         
            let sessions = updateWithSuccess sessionId (fun _ -> session) currentModel.Sessions
            let pass = session.Registrations |> List.tryFind (fun r -> samePersonTest r.Registrant currentModel.CurrentUser ) |> Option.bind (fun r -> r.Pass)
            let userPasses = 
                match pass with
                | Some p -> (p.PassNumber, session.Details) :: currentModel.UserPasses 
                            |> List.distinctBy (snd) |> List.sortBy (snd >> (fun d -> d.Date)) 
                | None -> currentModel.UserPasses |> List.filter (snd >> fun d -> d.Name <> session.Details.Name)
            let newModel = { currentModel with Sessions = sessions; UserPasses = userPasses }        
            newModel
        | Error exn -> 
            let sessions = updateWithErrors sessionId  (getResource currentModel "NoLoad") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            raise (exn)
            newModel
        | Ok(None) -> 
            let sessions = updateWithErrors sessionId  (getResource currentModel "NoSession") currentModel.Sessions
            let newModel = { currentModel with Sessions = sessions }        
            newModel
    match newModel.FocusTarget with
    | Some target -> 
        let cmd = Cmd.ofAsync Async.Sleep 30 (fun _ -> Focus target) (fun _ -> Focus target)
        newModel , cmd      
    | _ -> newModel, Cmd.none
let validateLogin (currentModel : Model) = 
    let login = 
        if String.IsNullOrWhiteSpace(currentModel.LoginName) then
            Error (getResource currentModel "NameRequired")
        else Ok currentModel.LoginName
    let pin = 
        if String.IsNullOrWhiteSpace(currentModel.Pin) then
            Error  (getResource currentModel "PINRequired")
        else 
            Ok currentModel.Pin
    match login,pin with
    | Ok login, Ok pin -> Ok(login,pin)
    | Ok _, Error e2 -> Error(None, Some e2)
    | Error e, Ok _ -> Error(Some e, None)
    | Error e, Error e2 -> Error(Some e, Some e2)
let validateNewSession currentModel =
    let (text,textFr,date,time,timeEnd,gap) = currentModel.NewSession 

    let date = 
        if String.IsNullOrWhiteSpace(date) then 
            Error (getResource currentModel "DateRequired")
        else 
            match System.DateTime.TryParse(date) with
            | true, _ -> Ok date
            | false, _ -> Error (getResource currentModel "DateWrongFormat")
    let time = 
        if String.IsNullOrWhiteSpace(time) then 
            Error (getResource currentModel "TimeRequired")
        else 
            match System.DateTime.TryParse(time) with
            | true, _ -> Ok time
            | false, _ -> Error (getResource currentModel "TimeWrongFormat")
    let gap = 
        if String.IsNullOrWhiteSpace(gap) then 
            Error (getResource currentModel "BreakTimeRequired")
        else 
            match System.Double.TryParse(gap) with
            | true, gap -> Ok gap
            | false, _ -> Error (getResource currentModel "BreakTimeFormat")
    let timeEnd = 
        if String.IsNullOrWhiteSpace(timeEnd) then 
            Error (getResource currentModel "TimeRequired")
        else 
            match System.DateTime.TryParse(timeEnd) with
            | true, time -> Ok timeEnd
            | false, _ -> Error (getResource currentModel "TimeWrongFormat")
        
    let text = 
        match text with
        | "" -> Error (getResource currentModel "EnglishDescRequired")
        | _ -> Ok text

    let textFr = 
        match textFr with
        | "" -> Error (getResource currentModel "FrenchDescRequired")
        | _ -> Ok textFr

    { currentModel with NewSessionErrorDate = date
                        NewSessionErrorText = text
                        NewSessionErrorTextFr = textFr
                        NewSessionErrorEndTime = timeEnd
                        NewSessionErrorTime = time
                        NewSessionErrorGap = gap
    }
let swapLocal local = 
    if local = englishUK then french, "fr", Navigation.modifyUrl "?lang=fr"
    else englishUK, "en", Navigation.modifyUrl "?lang=en"
let makeSecureRequest (model:Model) x = 
    {Content = x; Token = model.Token |> Option.defaultValue (SecurityToken "") }
let validateSecureRequest model r = 
    Result.mapError(function | TokenInvalid -> Exception(getResource model "Token validation Error")
                             | UserDoesNotHaveAccess -> Exception(getResource model "User not authenticated")) r


let loadProfile model token = 
    match token with 
    | Some token->
        Cmd.ofAsync
            Server.api.getProfile
            (makeSecureRequest model token)
            (LoadedProfile)
            (fun _ ->  LoadedProfile(Error(UserDoesNotHaveAccess)))
    | None -> Cmd.none
// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    let currentModel = { model with GlobalError = None }
    match msg with
    | Focus oldHeight -> 
       // try
        
        let element = Fable.Import.Browser.document.getElementById "YourPasses"
        let newHeight = if isNull element then 0. else element.offsetHeight;
        let delta = newHeight - oldHeight
        let x = Fable.Import.Browser.window.pageXOffset
        let y = Fable.Import.Browser.window.pageYOffset
        //printfn "%A" (x,y + delta)
        Fable.Import.Browser.window.scrollTo(x,y + delta)
        //with _ -> ()
        { currentModel with FocusTarget = None }, Cmd.none
    | StoreFocus (target) ->
       { currentModel with FocusTarget = Some target }, Cmd.none
    | Logout ->
        Fable.Import.Browser.document.cookie <- ("token=")
        BrowserLocalStorage.delete "token"
        { currentModel with CurrentUser = None; Token = None; Pin = "" }, Cmd.none 
    | TryGetToken ->    
        let c = Fable.Import.Browser.document.cookie
        let token = 
            c.Split(';')
            |> Array.tryFind(fun kv -> kv.Trim().StartsWith("token="))    
            |> Option.map(fun s -> s.Trim().Substring(6) |> SecurityToken)
            |> Option.filter (fun (SecurityToken token) -> String.IsNullOrWhiteSpace(token) |> not)
        
        let tokenLocal = 
            BrowserLocalStorage.load Thoth.Json.Decode.string "token"   
            |> function | Ok token -> Some (SecurityToken token)
                        | Error _ -> None
        let eitherToken =  Option.orElse tokenLocal token 

        let newModel = { currentModel with Token = eitherToken }
        newModel, loadProfile newModel eitherToken
    | LoggedIn(LoginSuccess (SecurityToken token as t)) -> 
        Fable.Import.Browser.document.cookie <- "token=" + token + ";max-age=" + (string (60*60*24*90))
        BrowserLocalStorage.save "token" token
        let newModel = { currentModel with Token = Some t; ForgetPin = false }
        newModel, loadProfile newModel newModel.Token
    | LoggedIn (LoginException exn) -> 
        Fable.Import.Browser.document.cookie <- ("token=")
        BrowserLocalStorage.delete "token"
        { currentModel with Token = None; GlobalError = Some (exn.Message)}, Cmd.none
    | LoggedIn (LoginError CouldNotFindUser) ->
        Fable.Import.Browser.document.cookie <- ("token=")
        BrowserLocalStorage.delete "token"
        { currentModel with Token = None; GlobalError = Some (getResource model "CouldNotFindUser")}, Cmd.none
    | LoggedIn (LoginError PinIncorrect) ->
        Fable.Import.Browser.document.cookie <- ("token=")
        BrowserLocalStorage.delete "token"
        { currentModel with Token = None; Pin = ""; PinError = Error (getResource model "PinIncorrect")}, Cmd.none
    | Login loginInfo -> 
        let loginCommand = 
            Cmd.ofAsync 
                Server.api.login 
                loginInfo
                (fun r -> match r with 
                          | Ok token -> LoginSuccess token  |> LoggedIn
                          | Error loginError -> LoginError loginError |> LoggedIn)
                (LoginException >> LoggedIn)
        {currentModel with Pin = ""}, loginCommand
    | AskToResetPin -> 
        { currentModel with ForgetPin = true; PinError = Ok "" }, Cmd.none
    | LoadedProfile (Ok registrant) -> 
        let loadSessionCmd =     
            Cmd.ofAsync
                initialPasses
                ()
                (Ok >> SessionsLoaded)
                (Error >> SessionsLoaded)
        { currentModel with CurrentUser = Some registrant }, loadSessionCmd
    | LoadedProfile (Error (TokenInvalid)) -> 
        { currentModel with GlobalError = Some (getResource model "Token validation Error")}, Cmd.none     
    | LoadedProfile (Error (UserDoesNotHaveAccess)) -> 
        { currentModel with GlobalError = Some (getResource model "User not authenticated")}, Cmd.none    
    | SetHtmlLanguage lang -> 
        try 
            let item =  Fable.Import.Browser.document.getElementsByTagName_html().item (0.)
            item.lang <- lang
        with _ -> ()
        currentModel, Cmd.none      
    | SwitchLanguage -> 
        let local,lang,navigationCmd = swapLocal currentModel.Local
        {currentModel with Local = local}, Cmd.batch[ navigationCmd; Cmd.ofMsg (SetHtmlLanguage lang)]
    | SessionsLoaded msg -> 
        //currentModel, Cmd.none    
        updateSessionsLoad msg currentModel
    | CancelReservation sessionId -> 
        let cancelCommand = 
                Cmd.ofAsync 
                    Server.api.cancel 
                    (makeSecureRequest currentModel sessionId)
                    (fun r ->
                        let updateResult = r |> validateSecureRequest currentModel |> Result.map Can
                        SessionUpdated(sessionId, updateResult))
                    (fun exn -> SessionUpdated(sessionId, Error(exn)))
        { currentModel with Sessions = updateWithLoading sessionId currentModel.Sessions }, cancelCommand
    | Reserve sessionId -> 
        let reserveCommand = 
                Cmd.ofAsync 
                    Server.api.reserve 
                    (makeSecureRequest currentModel sessionId)
                    (fun r ->
                        let result = r |> validateSecureRequest currentModel |> Result.map Reg 
                        SessionUpdated(sessionId, result))
                    (fun exn -> SessionUpdated(sessionId, Error(exn)))

        { currentModel with Sessions = updateWithLoading sessionId currentModel.Sessions }, reserveCommand
    | Refresh sessionId -> 
        let refreshSession = 
            Cmd.ofAsync 
                Server.api.getSession 
                sessionId 
                (fun session -> SessionLoaded(Some sessionId, Ok(session))) 
                (fun exn -> SessionLoaded(Some sessionId, Error(exn)))
        { currentModel with Sessions = updateWithLoading sessionId currentModel.Sessions }, refreshSession
    |  SessionUpdated (sessionId, msg) -> 
        let updateState, newCmd = sessionUpdated sessionId msg currentModel 
        let sessions = updateWithLoading sessionId currentModel.Sessions
        let newState = { updateState with Sessions = sessions }
        let refreshSession = 
            Cmd.ofAsync 
                Server.api.getSession 
                sessionId 
                (fun session -> SessionLoaded(Some sessionId, Ok(session))) 
                (fun exn -> SessionLoaded(Some sessionId, Error(exn)))
        newState, Cmd.batch [ refreshSession; newCmd ]
    |  SessionLoaded (Some sessionId, msg)-> 
        let newModel,newCmd = sessionLoaded sessionId msg currentModel 
        { newModel with AddingLoading = false }, newCmd
    |  SessionLoaded (None, (Ok(Some session) as msg)) -> 
        let newModel,newCmd = sessionLoaded session.Id msg currentModel 
        { newModel with AddingLoading = false }, newCmd
    |  SessionLoaded (None, Ok(None)) -> 
        {currentModel with AddingLoading = false; GlobalError = Some  (getResource currentModel "Couldn'tFindSession") },  Cmd.none
    | SessionLoaded (None, Error exn) -> 
        {currentModel with AddingLoading = false; GlobalError = Some  <| sprintf "%s %s" (getResource currentModel "Couldn'tAddSession") exn.Message },  Cmd.none
    | AddSession(text, textFr, date, endDate, gap) ->
        let newCmd =
            let sessionDetails = 
                { Session.defaultSession with Details = {Name = text; NameFr = textFr; Date = date; EndDate = endDate; Gap = gap } }
                |> makeSecureRequest currentModel
            Cmd.ofAsync
                Server.api.addSession
                sessionDetails
                (fun r ->
                    let result = r |> validateSecureRequest currentModel
                    SessionLoaded(None, result))
                (fun exn ->  SessionLoaded(None, Error(exn)))
        let (_,_,d,t,te,g) = currentModel.NewSession
        { currentModel with AddingLoading = true; NewSession = "", "", d, t, te,g }, newCmd
    | ValidateLogin -> 
        match validateLogin currentModel with
        | Ok (login,pin) -> 
            let loginInfo = {Username = {Name = login}; Pin = pin}
            let msg = 
                if currentModel.ForgetPin then ForgetMyPin 
                else Login
            // let setDate = 
            //     Cmd.ofAsync 
            //         Server.api.fixDate
            //         ("2019-05-22T07:00:00.000-04:00","2019-05-22T07:00:00.000-04:00",16) 
            //         (fun _ -> SessionUpdated(16, Ok(Reg(ReserveError(UpdateError)))))
            //         (fun _ -> SessionUpdated(16, Ok(Reg(ReserveError(UpdateError)))))
            let cmds = 
                Cmd.batch [
                    // setDate
                    Cmd.ofMsg (msg loginInfo)
                ]
            {currentModel with LoginError = Ok ""; PinError = Ok "" }, cmds
        | Error (namemsg,pinmsg) -> 
            {currentModel with
                LoginError = match namemsg with | Some e -> Error e | _ -> Ok "";
                PinError = match pinmsg with | Some e -> Error e | _ -> Ok "" }, Cmd.none
    | ValidateNewSession ->
        let newModel = 
            validateNewSession currentModel 
        let toOpt =  function Ok _ -> None | Error e -> Some e
        let toDate = function Ok d -> d | _ -> DateTime.MinValue
        let validationList =   
            [ toOpt newModel.NewSessionErrorText 
              toOpt newModel.NewSessionErrorTextFr  
              toOpt newModel.NewSessionErrorDate   
              toOpt newModel.NewSessionErrorTime 
              toOpt newModel.NewSessionErrorEndTime  ] |> List.choose id     
        if validationList.Length = 0 then
            let (e,f,d,t,et,g) = newModel.NewSession 
            let ds = d + "T" + t + ":00.000-04:00"
            let de = d + "T" + et + ":00.000-04:00"
            newModel,Cmd.ofMsg (AddSession(e,f,ds,de,float g)) 
        else 
            newModel, Cmd.none
    | DescriptionChanged sessionName -> 
        let (_,f,d,t,et,g) = currentModel.NewSession
        {currentModel with NewSession = sessionName, f, d,t,et,g }, Cmd.none         
    | DescriptionChangedFr sessionName -> 
        let (n,_,d,t,et,g) = currentModel.NewSession
        {currentModel with NewSession = n, sessionName, d,t,et,g }, Cmd.none
    | PinChanged p -> 
        let newStr = p.ToCharArray() |> Array.filter (fun c -> c >= '0' && c <= '9') |> String
        {currentModel with Pin = newStr }, Cmd.none
    | NameChanged name -> 
        {currentModel with LoginName = name }, Cmd.none
    | DateChanged date -> 
        let (n,f,_,t,et,g) = currentModel.NewSession
        {currentModel with NewSession = n,f, date,t,et ,g }, Cmd.none
    | GapChanged gap -> 
        let (n,f,d,t,et,_) = currentModel.NewSession
        {currentModel with NewSession = n,f, d,t,et, gap }, Cmd.none
    | TimeChanged time -> 
        let (n,f,d,_,et,g) = currentModel.NewSession
        {currentModel with NewSession = n,f, d,time,et ,g }, Cmd.none
    | EndTimeChanged time -> 
        let (n,f,d,t,_,g) = currentModel.NewSession
        {currentModel with NewSession = n,f, d,t,time ,g }, Cmd.none
    | ForgetMyPin loginInfo ->
        let resetCmd = 
            Cmd.ofAsync
                Server.api.resetPassword
                loginInfo
                (fun r -> match r with 
                          | Ok token -> LoginSuccess token  |> LoggedIn
                          | Error loginError -> LoginError loginError |> LoggedIn)
                (LoginException >> LoggedIn)
        currentModel, resetCmd
    | CancelResetPin ->
        { currentModel with ForgetPin = false; PinError = Ok ""; Pin = "" }, Cmd.none
    // | _, msg -> { currentModel with GlobalError = Some (sprintf "Couldn't parse the message %A because user is not logged in" msg) }, Cmd.none
