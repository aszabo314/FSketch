// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace Tg

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.Rendering
open Aardvark.Rendering.GL

open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

open Aardvark.Application
open Aardvark.Application.WPF       
open Aardvark.Application.WPF     

open System
open System.Windows

module Program =
    module Stuff =

        let rendering() = 
                
            let rc = RenderControl()
            let oglapp = new OpenGlApplication()
            oglapp.Initialize(rc, 16)

            let frustum = 
                adaptive {
                    let! ar = rc.Sizes
                    return Frustum.perspective 60.0 0.1 100.0 (float ar.X/float ar.Y)
                }

            let initialCam = CameraView.LookAt (V3d(0.0,0.0,0.0), V3d(6.0,1.0,1.0))

            let cam = 
                DefaultCameraController.control rc.Mouse rc.Keyboard rc.Time initialCam


            
            let index = [|0;1;2; 0;2;3|]
            let positions = [|V3f(-1,-1,0); V3f(1,-1,0); V3f(1,1,0); V3f(-1,1,0) |]

            let sg =
                IndexedGeometry(IndexedGeometryMode.TriangleList, index, SymDict.ofList [DefaultSemantic.Positions, positions :> Array], SymDict.empty)
                    |> Sg.ofIndexedGeometry
                    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.White |> toEffect]
                    |> Sg.viewTrafo ( cam |> Mod.map ( fun v -> CameraView.viewTrafo v ))
                    |> Sg.projTrafo ( frustum |> Mod.map ( fun v -> Frustum.projTrafo v))

            rc.RenderTask <- oglapp.Runtime.CompileRender(rc.FramebufferSignature, BackendConfiguration.NativeOptimized, sg)
            rc
        

    [<EntryPoint; STAThread>]
    let main argv = 

        Ag.initialize()
        Aardvark.Init()
        Mod.initialize()

        let csapp = CsharpApplication()
        let win = MainWindow()

        let r = Stuff.rendering()

        win.renderingcontrol.Content <- r :> obj

        csapp.Run(win)
