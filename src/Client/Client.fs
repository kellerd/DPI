module Client

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

open Elmish
open Elmish.React
open Elmish.Browser
open Elmish.Browser.Navigation
open Fable.PowerPack.Date.Local
open Types
open State
open View
open System

type Page = Language of string option

let pageParser : UrlParser.Parser<Page->Page, _> =
  let (<?>) = UrlParser.(<?>)
  UrlParser.map Language (UrlParser.top <?> UrlParser.stringParam "lang") 

let urlParser location = UrlParser.parsePath pageParser location        

let urlUpdate (result:Page option) (model:Model) =
    match result with
    | Some (Language(Some lang)) when lang.StartsWith("e", StringComparison.InvariantCultureIgnoreCase ) || 
                                      lang.StartsWith("ang", StringComparison.InvariantCultureIgnoreCase ) ->
        { model with Local = englishUK }, Cmd.batch [Cmd.ofMsg TryGetToken; Cmd.ofMsg (SetHtmlLanguage "en")]// Issue some search Cmd instead

    | Some (Language(Some lang))  when lang.StartsWith("f", StringComparison.InvariantCultureIgnoreCase ) ->
        { model with Local = french }, Cmd.batch [Cmd.ofMsg TryGetToken; Cmd.ofMsg (SetHtmlLanguage "fr") ]
    | lang ->
        System.Console.WriteLine (lang);
        ( { model with Local = englishUK }, Cmd.batch [Cmd.ofMsg TryGetToken; Navigation.modifyUrl "?lang=en";Cmd.ofMsg (SetHtmlLanguage "en") ] ) // no matching route - go home

let init page =
    urlUpdate page (State.init ())


Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate

#if DEBUG
//|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
