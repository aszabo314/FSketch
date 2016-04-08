﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Tg

open System
open System.Windows

open Aardvark.Base
open Aardvark.Base.Incremental     

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
        let scale =         XAMLHelpers.terrainScaleInput win
        let sigma =         XAMLHelpers.sigmaInput win
        let roughness =     XAMLHelpers.roughnessInput win
        let flatness =      XAMLHelpers.flatnessInput win

        let floor = Terrain.withParams terrainlevel sigma roughness flatness

        let rendering = Visuals.Scenegraph.ofFloor floor scale

        //RenderControl is a WPF ContentControl that contains some OpenGL rendering output. 
        //Set it as the child of some other visible control to display it.
        win.renderingcontrol.Content <- rendering :> obj

        csapp.Run(win)
