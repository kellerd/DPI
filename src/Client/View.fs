module View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open Shared
open Fulma
open Fable.FontAwesome
open Fable.PowerPack
open Fable.PowerPack.Date.Local

let dateToColour (date:System.DateTime) = 
   match date.DayOfWeek with
   | System.DayOfWeek.Wednesday -> Color.IsPrimary
   | System.DayOfWeek.Thursday -> Color.IsInfo
   | System.DayOfWeek.Friday -> Color.IsCustomColor "friday"
   | System.DayOfWeek.Tuesday -> Color.IsBlack
   | System.DayOfWeek.Monday -> Color.IsWarning
   | System.DayOfWeek.Saturday -> Color.IsGreyLight
   | System.DayOfWeek.Sunday -> Color.IsGreyDark
   | _ -> Color.IsBlack
let dateToColourS (d:string) = System.DateTime.Parse(d) |> dateToColour

let mobileToggle model dispatch =       
    a [
        OnClick (fun _ -> dispatch SwitchLanguage)
        Lang (if model.Local = englishUK then "fr" else "en")
        Class "has-text-primary is-hidden-desktop";  ] [str (getResource model "AlternateLanguage")]


let greetings model dispatch = 
    [ str (getResource model "Hello")
      model.CurrentUser |> Option.map (fun cu -> str cu.Name) |> ofOption
      str " "
      a [
        OnClick (fun _ -> dispatch Logout)
        Class "has-text-primary";  ] [str (getResource model "NotMe")]

      str " "
      mobileToggle model dispatch
    ] |> ofList


let isSelected = Class "is-selected" :> IHTMLProp
let fullDateFormat model date = Date.Format.localFormat model.Local "yyyy-MM-dd HH:mm" date

let cuteDateFormat model date = 
    if model.Local = englishUK then Date.Format.localFormat model.Local "dddd, MMMM dd hh:mm tt" date
    else Date.Format.localFormat model.Local "dddd, dd MMMM HH \h mm" date
        
let cuteDateFormatS model stringDate = System.DateTime.Parse(stringDate) |> cuteDateFormat model

let justTimeFormat model date =
    if model.Local = englishUK then Date.Format.localFormat model.Local " - hh:mm tt" date
    else Date.Format.localFormat model.Local " - HH \h mm" date

let justTimeFormatS model stringDate = System.DateTime.Parse(stringDate) |> justTimeFormat model


let justTimeFormatR model date =
    if model.Local = englishUK then Date.Format.localFormat model.Local "hh:mm tt -" date
    else Date.Format.localFormat model.Local "HH\hmm - " date

let justTimeFormatRS model stringDate = System.DateTime.Parse(stringDate) |> justTimeFormat model
      

let stdDateFormat model date = Date.Format.localFormat model.Local "yyyy-MM-dd" date

let showRegistration model (registration) = 
    // tr [] [ td [] [ registration.Pass |> Option.map (fun p -> sprintf "%d" p.PassNumber |> str) |> ofOption ]
            td [] [ str registration.Registrant.Name ]
            // td [] [ str (getResource model (sprintf "%A" registration.Status)) ] ]

let showInfoOnly (dispatch : Msg -> unit) model {Details = details; Description = en,fr} = 
    
    let date = details.Date
    let endDate = details.EndDate
    Hero.hero [ Hero.Color (dateToColourS details.Date)
                Hero.IsBold ] [ Hero.head [] []
                                Hero.body [] [ Container.container [] [ yield Heading.p 
                                                                                [Heading.Modifiers [Modifier.TextColor IsWhite]] 
                                                                                [ 
                                                                                  if model.Local = englishUK then 
                                                                                      yield str details.Name
                                                                                  else yield str details.NameFr ]
                                                                        
                                                                        yield Heading.p 
                                                                            [ Heading.IsSubtitle
                                                                              Heading.Modifiers [Modifier.IsPaddingless; Modifier.TextColor IsWhite] ] 
                                                                            [ cuteDateFormatS model date |> str
                                                                              justTimeFormatS model endDate |> str ]
                                                                        
                                                                        if model.Local = englishUK then 
                                                                            yield en
                                                                        else yield fr ] ] ]    
let showSessionPasses (dispatch : Msg -> unit) model session = 
    let registrations = 
        // let additional = 3 - (session.Session.FreePasses.Length + session.Session.Registrations.Length) % 3
        // let freeSessions = Array.init (session.Session.FreePasses.Length) (fun _ -> td [][ str (getResource model "Free")])
        // let blankSessions = Array.init (additional) (fun _ -> td [][])
        let additional = 3 - (session.Session.Registrations.Length) % 3
        let freeSessions = [||]
        let blankSessions = Array.init (additional) (fun _ -> td [][])
        let reserved = session.Session.Registrations |> List.map (showRegistration model) |> List.toArray
          
        Array.concat [reserved; freeSessions; blankSessions]
        |> Array.chunkIt 3
        |> Array.toList
        |> List.map (fun tds -> tr [] tds )

    let alt s = 
        if model.Local = englishUK then (getResource model s) + " " + session.Session.Details.Name |> Title
        else (getResource model s) + " " + session.Session.Details.NameFr |> Title
    let reserveButton = 
        let reserveed = session.Session.Registrations |> List.exists (fun r -> State.samePersonTest r.Registrant model.CurrentUser )
        if reserveed then 
            
            Button.button [ //Button.IsOutlined
                            (if session.Session.Registrations |> List.exists (fun r -> State.samePersonTest r.Registrant model.CurrentUser) then 
                                 Button.Color Color.IsWarning
                             else Button.Disabled true)
                            Button.IsLoading session.Loading
                            Button.OnClick(fun e -> dispatch (CancelReservation session.Session.Id)
                                                    let element = Fable.Import.Browser.document.getElementById "YourPasses"
                                                    let oldHeight = if isNull element then 0. else element.offsetHeight;
                                                    dispatch (StoreFocus oldHeight)
                                                    )
                            Button.Props [ alt "CancelReservationAlt"] 
                            
                            Button.Size IsLarge ] 
                          [ Icon.icon [] [ Fa.i [ Fa.Solid.UserMinus ] [] ]
                            span [] [ str (getResource model "CancelReservation") ] ]
        else 
            Button.button [ //Button.IsOutlined
                            (if session.Session.FreePasses.Length > 0 then 
                                 Button.Color Color.IsSuccess
                             else Button.Disabled true)
                            Button.IsLoading session.Loading
                            Button.OnClick(fun e -> dispatch (Reserve session.Session.Id)
                                                    let element = Fable.Import.Browser.document.getElementById "YourPasses"
                                                    let oldHeight = if isNull element then 0. else element.offsetHeight;
                                                    dispatch (StoreFocus oldHeight)
                                                    )

                            Button.Props [ alt "ReserveAlt"] 
                            Button.Size IsLarge ]
                          [ Icon.icon [] [ Fa.i [ Fa.Solid.UserPlus ] [] ]
                            span [] [ str (getResource model "Reserve") ] ]
    
    let refreshButton = 
        Button.button [ //Button.IsOutlined
                        (if session.Loading then Button.Disabled true
                         else Button.Color Color.IsPrimary)
                        Button.OnClick(fun _ -> dispatch (Refresh session.Session.Id))
                        Button.Props [ alt "Refresh" ]
                      //  Button.Size IsLarge
                        Button.Modifiers [Modifier.IsPulledRight; ];
                       ] [ Icon.icon [] [ Fa.i [ Fa.Solid.Sync
                                                 (if session.Loading then Fa.Spin
                                                  else Fa.CustomClass "") ] [] ] ]
    let count = 
        [   str " "
            (session.Session.Registrations.Length) |> string |> str
            str "/"
            (session.Session.Registrations.Length + session.Session.FreePasses.Length) |> string |> str
            str " "
        ] |> b [Class "tag  is-info is-pulled-right is-medium"; alt "CountAlt"; Style [MarginRight 5; MarginLeft 5]]                                             
    
    let registrationTable = 
        Table.table [ Table.IsFullWidth; Table.IsStriped  ] [ thead [] [ tr [] [ th [ColSpan 3] [ str (getResource model "Reservations") ; refreshButton; count;  ] ] ]
                                                              tbody [] registrations ]
    
    let isFull = 
        if session.Session.FreePasses.Length = 0
           && List.exists (fun r -> State.samePersonTest r.Registrant model.CurrentUser |> not) session.Session.Registrations then 
            Some(getResource model "SessionFull")
        else None
    
    let showMessage = 
        let messageClass = 
            if List.isEmpty session.MessageClass then [ Message.Color IsWarning ]
            else session.MessageClass
        session.Message
        |> Option.orElse isFull
        |> Option.map 
               (fun s -> 
               Columns.columns [ Columns.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] 
                   [ Column.column [ Column.Width(Screen.All, Column.Is9) ] 
                         [ Message.message messageClass [ Message.header [] [ str "" ]
                                                          Message.body [] [ str s ] ] ] ])
        |> ofOption
    
    let mainContent = 
        Columns.columns [ Columns.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] 
            [ Column.column [ Column.Width(Screen.All, Column.Is9) ] [ registrationTable ]
              Column.column [ Column.Width(Screen.All, Column.Is3) ] [ reserveButton ] ]
    
    let date = session.Session.Details.Date
    let endDate = session.Session.Details.EndDate
    Hero.hero [ Hero.Color (dateToColourS session.Session.Details.Date)
                Hero.IsBold ] [ Hero.head [ ] []
                                Hero.body [] [ Container.container [] [ Heading.p [Heading.Props  [Id (sprintf "Session%d" session.Session.Id)]; 
                                                                                   Heading.Modifiers [Modifier.TextColor IsWhite]] 
                                                                            [ 
                                                                              if model.Local = englishUK then 
                                                                                  yield str session.Session.Details.Name
                                                                              else yield str session.Session.Details.NameFr
                                                                              yield str " "
                                                                              yield a [ Href (getResource model "SessionInfoLink")
                                                                                        Class "button is-small"
                                                                                        Target "_blank"
                                                                                        alt "SessionInfoAlt" ] 
                                                                                      [ str (getResource model "SessionInfo")] ]
                                                                        
                                                                        Heading.p 
                                                                            [ Heading.IsSubtitle
                                                                              Heading.Modifiers [Modifier.IsPaddingless; Modifier.TextColor IsWhite] ] 
                                                                            [ cuteDateFormatS model date |> str
                                                                              justTimeFormatS model endDate |> str ]
                                                                        mainContent
                                                                        showMessage ] ] ]

//[heroTitle;mainContent] |> ofList
let globalMessage model = 
    model.GlobalError
    |> Option.map (fun globalError -> 
           Message.message [ Message.Color IsWarning ] [ Message.header [] [ str "" ]
                                                         Message.body [] [ str globalError ] ]
           |> List.singleton
           |> Container.container []  |> List.singleton
           |> Hero.body []  |> List.singleton
           |> Hero.hero [ Hero.Color Color.IsPrimary ])
    |> ofOption

let show dispatch model = 
    match model with
    | { Sessions = Some sessions } -> 
        sessions
        |> Map.toList
        |> List.sortBy (snd >> function InfoOnly s -> s.Details.Date | CanRegister s -> s.Session.Details.Date)
        |> List.map (snd >> function CanRegister s -> showSessionPasses dispatch model s
                                     | InfoOnly s -> showInfoOnly dispatch model s)
        |> Fable.Helpers.React.ofList
    | { Sessions = None } ->
        let loading = 
            Tile.tile [Tile.IsVertical; Tile.CustomClass "notification is-info"; ] 
                [ Control.p [Control.IsLoading true] [ str (getResource model "Loading") ] ] 
        
        Hero.hero [ Hero.Color Color.IsPrimary ] 
            [ Hero.head [] []
              Hero.body [] [ Container.container [] [ loading ] ] ]

let navBrand model = 
    //None |> ofOption
    Navbar.menu [] 
        [ Navbar.Brand.div [] 
                    [ Navbar.Item.div [Navbar.Item.CustomClass "title"] [ str (getResource model "DPI Registration") ] ] ] 
    // Navbar.Brand.div [] [ Navbar.Item.a [ Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
    //                                       Navbar.Item.IsActive true ] 
    //                                     [ img [ Src "https://safe-stack.github.io/images/safe_top.png"
    //                                             Alt (getResource model "Logo") ] ] ]

let navMenu model dispatch = 
    Navbar.menu [] 
        [ Navbar.End.div [] 
              [                 
                Navbar.Item.a [ Navbar.Item.Props [ OnClick(fun _ -> dispatch SwitchLanguage) ] ] 
                    [ str (getResource model "AlternateLanguage") ] ] ]

let tile color options title subtitle = 
    let content = 
        [ Heading.p [ Heading.IsSpaced ] title
          Content.content 
                    [ Content.CustomClass "is-subtitle" 
                      Content.Modifiers [Modifier.IsPaddingless] ] subtitle ]
    Notification.notification [Notification.Color color] content 
    |> List.singleton
    |> Column.column options  

type TimeSlot = { StartTime : System.DateTime; EndTime : System.DateTime; Sessions : SessionDetails list }    


let toTimeSlots userSessions = 
    let processTimeSlots sessions = 
        List.foldBack (fun (item:SessionDetails) acc -> 
            match acc with 
            | lastItem :: acc when lastItem.StartTime <= System.DateTime.Parse(item.EndDate).AddMinutes(item.Gap) -> 
                     { lastItem with StartTime = System.DateTime.Parse(item.Date); Sessions = item :: lastItem.Sessions } :: acc
            | acc -> { StartTime = System.DateTime.Parse(item.Date); EndTime = System.DateTime.Parse(item.EndDate); Sessions = [item]  }  :: acc
            
        ) sessions []

    userSessions 
    |> List.groupBy (fun (f:SessionDetails) -> System.DateTime.Parse(f.Date).Date)      
    |> List.collect (snd >> List.sortBy (fun f -> f.EndDate) >> processTimeSlots)
    |> List.sortBy(fun f -> f.EndTime)
let miniTile model details = li [] [str details.Name; justTimeFormatS model details.Date |> str] 
// let miniTile s = 
    
//     Tile.tile 
//          [Tile.IsVertical; Tile.CustomClass "notification is-small is-success"; ] 
//          [ Control.p [] [ str s ] ] 

let yourTimeSlots model = 
    if model.UserPasses.Length = 0 then None
    else
        let mainTile = 
            model.UserPasses
            |> List.map snd 
            |> toTimeSlots
            |> List.map 
                   (fun ({ StartTime = sd; EndTime = ed; Sessions = sessionList }  ) -> 
                   tile (dateToColour sd) 
                               [Column.Width(Screen.FullHD, Column.Is4)
                                Column.Width(Screen.Desktop, Column.Is4) 
                                Column.Width(Screen.WideScreen, Column.Is4)
                                Column.Width(Screen.Tablet, Column.Is4)
                                Column.Width(Screen.Mobile, Column.Is12   )] 
                       [ 
                            str (cuteDateFormat model sd) 
                            str (justTimeFormat model ed)  ] 
                       [ ul [] 
                            [ if model.Local = englishUK then yield! sessionList |> List.map (miniTile model)
                              else yield! sessionList |> List.map (miniTile model) ]] )
            |> div [Style [FlexWrap "wrap"]; Class "columns" ]

        let message = 
            Message.message 
                [ Message.Color IsWarning ] 
                [ Message.header [] [ str "" ] 
                  Message.body [] [ str (getResource model "Note") ] ]
            
        Hero.hero [ Hero.Color Color.IsPrimary; 
                    Hero.Props [Id "YourPasses"] ] 
                  [ Hero.head [] []
                    Hero.body [Props [Style[PaddingTop 0]]] 
                        [ Heading.p [] [ str (getResource model "YourPasses") ]
                          message
                          mainTile ] ]
        |> Some                                                   
    //|> ofOption
    |> Option.defaultValue (div [Id "YourPasses"; Style [MinHeight 0]] [])      
                                                
let containerBox (model : Model) (dispatch : Msg -> unit) = show dispatch model
let horizontalField id input label errorMessage = 
    Field.div [ Field.IsHorizontal ] [ Field.label [ Field.Label.IsNormal ] 
                                           [ Label.label [ Label.For id ; Label.Props [ Style [ Color "#FFF" ] ] ] [ str label ] ]
                                       Field.body [] [ Field.div [] [ yield Control.p [] [ input ]
                                                                      
                                                                      match  errorMessage with 
                                                                      | Error s-> 
                                                                        yield Help.help [ Help.Color Color.IsDanger; Help.Modifiers [Modifier.TextSize (Screen.All,TextSize.Is4)]  ] [ str s ] 
                                                                      | _ -> ()
                                                                    ] ] ]
let validatedField field (onChange : string -> unit) opts key placeholder label errorMessage item = 
    let input = 
        field [ yield! opts
                yield Input.Id key
                yield Input.Placeholder placeholder
                match  errorMessage with Error _ -> yield Input.Color Color.IsDanger | _ -> ()
                yield Input.OnChange(fun ev -> onChange ev.Value) ]
    horizontalField key input label errorMessage
let validatedTextBox (onChange : string -> unit) key placeholder label errorMessage item = 
    validatedField Input.text onChange [Input.Value item] key placeholder label errorMessage item
let validatedNumber (onChange : string -> unit) key placeholder label errorMessage item = 
    validatedField Input.number onChange [Input.Value item] key placeholder label errorMessage item
let validatedDateBox (onChange : string -> unit) key placeholder label errorMessage item = 
    validatedField Input.date onChange  [Input.DefaultValue item] key placeholder label errorMessage item
let validatedTimeBox (onChange : string -> unit) key placeholder label errorMessage item = 
    validatedField Input.time onChange  [Input.DefaultValue item] key placeholder label errorMessage item
let validatedPinBox model dispatch (onChange : string -> unit) key placeholder label errorMessage item = 
    let field = 
        validatedField 
            Input.password onChange  
            [ Input.Size IsLarge; Input.Props [MaxLength 3.; MinLength 3.; Pattern "[0-9]*"; InputMode "numeric" ]; Input.Value item] 
            key placeholder label errorMessage item
    let resetPin =     
          match errorMessage with 
          | Ok _ -> ofList []
          | Error _ -> 
              ofList [ str " "
                       Button.button [
                        Button.OnClick (fun _ -> dispatch AskToResetPin)
                        Button.CustomClass "has-text-primary input is-6";  ] [str (getResource model "ResetPin")] ] 
    [field; resetPin ] |> ofList
let addSessionBox model dispatch = 
    let form = 
        [ validatedTextBox (dispatch << DescriptionChanged) "title" (getResource model "PleaseSessionDesc") 
              (getResource model "SessionDescription") model.NewSessionErrorText (model.NewSession |> fun (s, _, _, _, _,_) -> s)
          
          validatedTextBox (dispatch << DescriptionChangedFr) "titleFr" (getResource model "PleaseSessionDescFr") 
              (getResource model "SessionDescriptionFr") model.NewSessionErrorTextFr (model.NewSession |> fun (_, f, _, _, _,_) -> f)
          
          validatedDateBox (dispatch << DateChanged) "sessionDate" "YYYY-MM-DD" (getResource model "SessionDate") 
              model.NewSessionErrorDate (model.NewSession |> fun (_, _, d, _, _,_) -> d)
          
          validatedTimeBox (dispatch << TimeChanged) "sessionTime" "HH:MM" (getResource model "SessionTime") 
              model.NewSessionErrorTime (model.NewSession |> fun (_, _, _, t, _,_) -> t)
              
          validatedTimeBox (dispatch << EndTimeChanged) "sessionTimeEnd" "HH:MM" (getResource model "SessionEndTime") 
              model.NewSessionErrorEndTime (model.NewSession |> fun (_, _, _, _, et,_) -> et)
    
          validatedTextBox (dispatch << GapChanged) "gap" (getResource model "BreakTimeHowLong") 
            (getResource model "BreakTime") model.NewSessionErrorGap (model.NewSession |> fun (_, _, _, _, _,g) -> g)

          Button.button [ Button.OnClick(fun _ -> dispatch ValidateNewSession)
                          Button.Color Color.IsWhite
                          Button.IsLoading model.AddingLoading
                          Button.Modifiers [Modifier.IsPulledRight] ] [ str (getResource model "AddSession") ] ]
    let box = 
        Hero.hero [ Hero.Color Color.IsDark
                    Hero.IsBold ] [ Hero.head [] []
                                    Hero.body [] [ Container.container [] [ yield Heading.p [] 
                                                                                      [ str (getResource model "Add a session") ]
                                                                            yield! form ] ] ]
    let schedule = 
        let link = 
            a [Href (getResource model "ScheduleLink")
               Target "_blank"
               Class "button is-primary is-large"] [ str (getResource model "Schedule")]
        Hero.hero [ Hero.Color Color.IsDark
                    Hero.IsBold ] [ Hero.head [] []
                                    Hero.body [] [ Container.container [Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ]] [ yield Heading.p [] 
                                                                                      [ link ] ] ] ]
    match model.CurrentUser with 
    | Some {Name = n} 
        when n.Equals("Danny Keller", System.StringComparison.InvariantCultureIgnoreCase) || 
             n.Equals("Marcel Paquet", System.StringComparison.InvariantCultureIgnoreCase) -> box
    | _ -> schedule

let loggedInHeader model dispatch = 
    let messageList =
        ol [Class "subtitle is-paddingless"] [
            li [] [ getResource model "ol1a"|> str
                    a [ Class "has-text-primary"
                        Target "_blank"
                        Href (getResource model "ol1b")] [ getResource model "ol1c"|> str ]
                    getResource model "ol1d"|> str]
            li [] [getResource model "ol3"|> str]
            li [] [ getResource model "ol2a"|> str
                    a [ Class "has-text-primary"
                        Target "_blank"
                        Href (getResource model "ol2b")] [ getResource model "ol2c"|> str ]
                    getResource model "ol2d"|> str
                   ]
        ]
        |> List.singleton
        |> Container.container []
    Hero.body [] 
        [ Container.container [ Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] 
              [ Column.column [ Column.Width(Screen.All, Column.Is10)
                                Column.Offset(Screen.All, Column.Is1) ] 
                    [   Heading.p [ Heading.IsSpaced
                                    Heading.Modifiers [ Modifier.IsHidden (Screen.Desktop, true) ] ] 
                                  [str (getResource model "DPI Registration")]
                        Heading.p [ Heading.IsSubtitle 
                                    Heading.Modifiers [Modifier.IsPaddingless] ] 
                                       [ greetings model dispatch ] ] ]
          messageList ]

let loggedOutHeader model dispatch = 
    Hero.body 
        [ GenericOption.Modifiers [ Modifier.IsHidden (Screen.Desktop, true) ] ]
        [ Container.container 
              [ Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] 
              [ Column.column [ Column.Width(Screen.All, Column.Is10)
                                Column.Offset(Screen.All, Column.Is1) ] 
                              [ Heading.p [ Heading.IsSpaced ]  
                                    [str (getResource model "DPI Registration")]
                                Heading.p [ Heading.IsSubtitle; Heading.Modifiers [Modifier.IsPaddingless] ] 
                                       [ mobileToggle model dispatch ] ] ] ]
                                    

let loggedInBody model dispatch = 
    [ yourTimeSlots model
      addSessionBox model dispatch
      containerBox model dispatch ]

let loggedOutBody model dispatch = 

    let loginForm = 
        [ Content.content [] [str (getResource model "Registration Msg")]
          validatedTextBox (dispatch << NameChanged) "name" (getResource model "EnterName") (getResource model "Name") model.LoginError model.LoginName
          validatedPinBox model dispatch (dispatch << PinChanged) "pin" (getResource model "EnterPin") (getResource model "PIN") model.PinError model.Pin 
          Input.input [ 
                        Input.Color Color.IsPrimary
                        Input.CustomClass "button is-6"
                        Input.Props[Type "submit"; OnClick(fun _ -> dispatch ValidateLogin)] 
                        Input.Value (getResource model "Login") ] ]
        |> form [OnSubmit (fun e -> e.preventDefault() )]
  
        
    let resetPinForm = 
        [ Content.content [] [
                              str model.LoginName
                              str ", "
                              str (getResource model "ChangeYourPin") ]
          validatedPinBox model dispatch (dispatch << PinChanged) "pin" (getResource model "NewPin") (getResource model "NewPin") model.PinError model.Pin 
          Input.input [ 
                        Input.Color Color.IsPrimary
                        Input.CustomClass "button is-6"
                        Input.Props[Type "submit"; OnClick(fun _ -> dispatch ValidateLogin)] 
                        Input.Value (getResource model "ResetPin") ]
          Button.button [
              Button.OnClick (fun _ -> dispatch CancelResetPin)
              Button.CustomClass "has-text-primary input is-6";  ] [str (getResource model "Cancel")] ]
        |> form [OnSubmit (fun e -> e.preventDefault() )]

    let formContent = if model.ForgetPin then resetPinForm else loginForm

    let formContainer = 
        Tile.tile [Tile.IsVertical; Tile.CustomClass "notification is-info"; ] 
            [ Heading.p [] [ str (getResource model "Introduce") ]
                          
              Content.content [] [ formContent ] ] 
              

    [ 
      Hero.hero [ Hero.Color Color.IsPrimary
                  Hero.Props [Style [PaddingTop 0] ] ] 
                [ Hero.head [] []
                  Hero.body [] [ Container.container [] [ formContainer ] ] ]
      Hero.hero [ Hero.Color Color.IsPrimary; Hero.IsFullHeight ] 
                [ Hero.head [] []; Hero.body [] [] ]
    ]

let header model = 
    match model.CurrentUser with
    | Some _ -> loggedInHeader model
    | None -> loggedOutHeader model

let body model dispatch = 
    let mainBody = 
        match model.CurrentUser with
        | Some _ -> loggedInBody model dispatch
        | None -> loggedOutBody model dispatch
    globalMessage model :: mainBody 
    |> main []

let view (model : Model) (dispatch : Msg -> unit) = 
    [ Hero.hero [ Hero.Color IsPrimary; Hero.IsFullheightWithNavbar ] 
                [ Hero.head [] 
                            [ Navbar.navbar [Navbar.Modifiers [Modifier.IsHiddenOnly(Screen.Mobile, true);   ]] [ //Container.container [] [ 
                                                                          navBrand model
                                                                          navMenu model dispatch// ] 
                                                                          ] ]
                  header model dispatch ]
      body model dispatch ]
    |> ofList