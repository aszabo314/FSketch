// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Tg

open System
open System.Windows

open Aardvark.Base
open Aardvark.Base.Incremental     
open Aardvark.Application.WPF

open Visuals

module Program =

    [<EntryPoint; STAThread>]
    let main argv = 

        Ag.initialize()
        Aardvark.Init()
        Mod.initialize()

        let csapp = CsharpApplication()
        let win = MainWindow()

        let terrainlevel =  XAMLHelpers.terrainGenerationLevelInput win
        let sigma =         XAMLHelpers.sigmaInput win
        let roughness =     XAMLHelpers.roughnessInput win
        let flatness =      XAMLHelpers.flatnessInput win

        let waterEnabled =  XAMLHelpers.waterEnabledInput win
        let colors =        Mod.constant ( [| C4f.Green; C4f.Yellow |], [| 0.5; 1.0 |] )

        let scale =         XAMLHelpers.terrainScaleInput win
        let height =        XAMLHelpers.terrainHeightInput win

        let floor = Terrain.withParams terrainlevel sigma roughness flatness

        let rendering : RenderControl = Visuals.Scenegraph.ofFloor floor scale height waterEnabled colors

        //RenderControl is a WPF ContentControl that contains some OpenGL rendering output. 
        //Set it as the child of some other visible control to display it.
        win.renderingcontrol.Content <- rendering :> obj

        csapp.Run(win)
