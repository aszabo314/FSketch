namespace Tg

module Visuals =
    
    open System

    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Rendering
    open Aardvark.Rendering.GL

    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics

    open Aardvark.Application
    open Aardvark.Application.WPF   
    
    open Terrain    

    module Scenegraph =
        
        //represents one rendering context consisting of:
        // - one RenderControl ( = what you see )
        // - one Camera with controls and frustum
        // - one function to set the RenderControl (hiding the compileRender call)
        module Context =   
            let private oglapp = new OpenGlApplication()

            let rc = 
                let rc = RenderControl()
                oglapp.Initialize(rc, 16)
                rc

            let frustum = 
                adaptive {
                    let! ar = rc.Sizes
                    return Frustum.perspective 60.0 0.1 100.0 (float ar.X/float ar.Y)
                }

            
            let cam = 
                let initialCam = CameraView.LookAt (V3d(0.0,0.0,0.0), V3d(6.0,1.0,1.0))
                DefaultCameraController.control rc.Mouse rc.Keyboard rc.Time initialCam
                    
            let setRender (sg : ISg) = 
                rc.RenderTask <- oglapp.Runtime.CompileRender(rc.FramebufferSignature, BackendConfiguration.NativeOptimized, sg)

        //this is a RenderControl that depends on one Floor as its content
        let ofFloor ( floor : IMod<Terrain.Floor> ) =
            
            let vfp ( x : FloorPoint ) =
                V3d( x.Pos.X, x.Pos.Y, x.Height )

            let rec floorGeometry ( min : FloorPoint ) ( max : FloorPoint ) ( after : Option<Fork<Floor>> ) =
                match after with
                    //there are no successors, the result is a Sg made of the current geometry (list of single element)
                    | None ->                                           
                        let topleftpoint = min                          |> vfp
                        let toprightpoint = min.XFullwayTowards max     |> vfp
                        let bottomleftpoint = min.YFullwayTowards max   |> vfp
                        let bottomrightpoint = max                      |> vfp

                        let positions = [| topleftpoint; toprightpoint; bottomrightpoint; bottomleftpoint |]
                        let indices = [|0;1;2; 0;2;3|]

                        IndexedGeometry(IndexedGeometryMode.TriangleList, indices, SymDict.ofList [DefaultSemantic.Positions, positions :> Array], SymDict.empty)
                            |> Sg.ofIndexedGeometry
                            |> List.singleton
                    //there are successors, our result is the list of their results
                    | Some after ->                                     
                        let TL = after.TopLeft
                        let TR = after.TopRight
                        let BL = after.BotLeft
                        let BR = after.BotRight
                        [ 
                            yield! floorGeometry TL.Min TL.Max TL.After     //yield! is list concatenation
                            yield! floorGeometry TR.Min TR.Max TR.After
                            yield! floorGeometry BL.Min BL.Max BL.After
                            yield! floorGeometry BR.Min BR.Max BR.After
                        ]
            let floorISg ( floor : Floor ) =
                floorGeometry floor.Min floor.Max floor.After |> Sg.group'

            let sg = 
                aset {
                    let! floor = floor
                    yield floorISg floor
                }   |> Sg.set
                    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.constantColor C4f.White |> toEffect]
                    |> Sg.viewTrafo ( Context.cam |> Mod.map ( fun v -> CameraView.viewTrafo v ))
                    |> Sg.projTrafo ( Context.frustum |> Mod.map ( fun v -> Frustum.projTrafo v))
            
            Context.setRender sg
            Context.rc
    

    module XAMLHelpers =
        module Converter =
            let intOfString (v : obj) =
                Int32.Parse(string obj)

        let terrainGenerationLevelInput (win : MainWindow) =
            let slider = win.terraingenerationlevelslider
            let res = int slider.Value |> Mod.init
            slider.ValueChanged.Add ( fun v -> transact ( fun _ -> int v.NewValue |> Mod.change res ))
            res :> IMod<_>
