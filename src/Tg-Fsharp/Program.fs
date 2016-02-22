// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Tg

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Rendering

open System
open System.Windows

module Program =
    module Stuff =
    
        let makeSomeMods () =

            let zahl = Mod.init 0

            let bam = 
                adaptive {
                    let! x = zahl
                    return (x+1)*15
                }
            zahl,bam
        

    [<EntryPoint; STAThread>]
    let main argv = 
        printfn "bam oida %A" argv
        Aardvark.Init()
        Mod.init() |> ignore

        let (a,b) = Stuff.makeSomeMods ()


        printfn "%A %A" (a |> Mod.force) (b |> Mod.force)
    
        transact( fun _ -> 123 |> Mod.change a)
    
        printfn "%A %A" (a |> Mod.force) (b |> Mod.force)

        let csapp = CsharpApplication()
        let win = MainWindow()

        let disposable = b |> Mod.unsafeRegisterCallbackKeepDisposable ( fun v -> win.contentcontrol.Content <- v )
        win.mainbutton.Click.Add( fun _ -> transact ( fun _ -> (a |> Mod.force) + 1 |> Mod.change a) )

        csapp.Run(win)
