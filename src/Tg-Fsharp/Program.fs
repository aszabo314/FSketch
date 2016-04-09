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
        let colorEnabled =  XAMLHelpers.colorEnabledInput win
        let (colors, colorRanges) = XAMLHelpers.colorsAndRanges win

        let scale =         XAMLHelpers.terrainScaleInput win
        let height =        XAMLHelpers.terrainHeightInput win

        let floor = Terrain.withParams terrainlevel sigma roughness flatness

        let rc : RenderControl = Visuals.Scenegraph.ofFloor floor scale height waterEnabled colorEnabled colors colorRanges

        //RenderControl is a WPF ContentControl that contains some OpenGL rendering output. 
        //Set it as the child of some other visible control to display it.
        win.renderingcontrol.Content <- rc :> obj

        //connect WPF display elements with  mods
        let camStatus = Visuals.Scenegraph.Context.cam |> Mod.map ( fun cv -> String.Format("camera = {0:00.00}",cv.Location) )
        XAMLHelpers.displayLabel camStatus win.camerapositionlabel |> ignore

        let highestHeight = floor |> Mod.map ( fun (_,_,h) -> String.Format("Highest Peak = {0:00.00}", h))
        let lowestHeight = floor |> Mod.map ( fun (_,h,_) -> String.Format("Lowest Valley = {0:00.00}", h))
        XAMLHelpers.displayLabel highestHeight win.maxheightlabel |> ignore
        XAMLHelpers.displayLabel lowestHeight win.minheightlabel |> ignore

        csapp.Run(win)
