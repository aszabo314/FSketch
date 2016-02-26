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

        type Orientation = 
            | Clockwise
            | Counterclockwise


        module RenderInfo =
            
            type RenderInfo =
                {
                    Positions : V3d[]
                    Normals   : V3d[]
                    Colors    : C4b[]
                } with 
                member this.Concat (other : RenderInfo) =
                    {
                        Positions = this.Positions  |> Array.append other.Positions
                        Normals   = this.Normals    |> Array.append other.Normals
                        Colors    = this.Colors     |> Array.append other.Colors
                    }

            let empty = 
                {
                    Positions = Array.empty
                    Normals   = Array.empty
                    Colors    = Array.empty
                }

        open RenderInfo
                    
                

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
                    return Frustum.perspective 60.0 0.1 1000.0 (float ar.X/float ar.Y)
                }

            let cam = 
                let initialCam = CameraView.LookAt (V3d(0.0,0.0,0.0), V3d(6.0,1.0,1.0))
                DefaultCameraController.control rc.Mouse rc.Keyboard rc.Time initialCam
                    
            let setRender (sg : ISg) = 
                rc.RenderTask <- oglapp.Runtime.CompileRender(rc.FramebufferSignature, BackendConfiguration.NativeOptimized, sg)

        //this is a RenderControl that depends on one Floor as its content
        let ofFloor ( floor : IMod<Terrain.Floor> ) ( scale : IMod<float> ) =
            
            let vfp ( x : FloorPoint ) =
                V3d( x.Pos.X, x.Pos.Y, x.Height )

            let norms (v0:V3d) v1 v2 =
                let oriented (n:V3d) =
                    if n.Dot( V3d.OOI ) > Math.PI/2.0 then V3d(n.X,n.Y,-n.Z) else n

                v0.Cross v1 |> oriented, v0.Cross v2 |> oriented

            let rec floorGeometry ( corners : Fork<FloorPoint> ) ( cw : Orientation ) ( after : Option<Fork<Floor>> ) =
                match after with
                    //there are no successors, the result is a Sg made of the current geometry (list of single element)
                    | None ->                         
                        let (positions, colors, normals) = 
                            let topleftpoint = corners.TopLeft      |> vfp
                            let toprightpoint = corners.TopRight    |> vfp
                            let bottomleftpoint = corners.BotLeft   |> vfp
                            let bottomrightpoint = corners.BotRight |> vfp

                            //orientation determines where the diagonal is (has to point toward the middle).
                            //all vertices appear twice since they have different normals.
                            match cw with
                                | Clockwise ->
                                    [| topleftpoint; toprightpoint; bottomrightpoint; topleftpoint; bottomrightpoint; bottomleftpoint |],
                                    [| C4b.White; C4b.White; C4b.White; C4b.White; C4b.White; C4b.White |],

                                    let v0 = (bottomrightpoint - topleftpoint).Normalized
                                    let v1 = (toprightpoint    - topleftpoint).Normalized
                                    let v2 = (bottomleftpoint  - topleftpoint).Normalized

                                    let (n012,n023) = norms v0 v1 v2

                                    [| n012; n012; n012; n023; n023; n023 |]
                                | Counterclockwise ->
                                    [| toprightpoint; bottomrightpoint; bottomleftpoint; toprightpoint; bottomleftpoint; topleftpoint |],
                                    [| C4b.White; C4b.White; C4b.White; C4b.White; C4b.White; C4b.White |],

                                    let v0 = (bottomleftpoint  - toprightpoint).Normalized
                                    let v1 = (bottomrightpoint - toprightpoint).Normalized
                                    let v2 = (topleftpoint     - toprightpoint).Normalized

                                    let (n012,n023) = norms v0 v1 v2

                                    [| n012; n012; n012; n023; n023; n023 |]

                        positions, normals, colors

                    //there are successors, our result is the list of their results
                    | Some after ->                                     
                        let TL = after.TopLeft
                        let TR = after.TopRight
                        let BL = after.BotLeft
                        let BR = after.BotRight
                        
                        let (tlp, tln, tlc) = floorGeometry TL.Corners Clockwise        TL.After
                        let (trp, trn, trc) = floorGeometry TR.Corners Counterclockwise TR.After
                        let (blp, bln, blc) = floorGeometry BL.Corners Counterclockwise BL.After
                        let (brp, brn, brc) = floorGeometry BR.Corners Clockwise        BR.After
                        [tlp; trp; blp; brp] |> Array.concat,
                        [tln; trn; bln; brn] |> Array.concat,
                        [tlc; trc; blc; brc] |> Array.concat

                        
            let floorISg ( floor : Floor ) =
                Report.BeginTimed("Creating Visualization ")
                let infos = floorGeometry floor.Corners Clockwise floor.After
                let res =
                    let (indexedAttributes, indices) = 
                        let (pos, norm, col) = infos
                        [
                            DefaultSemantic.Positions, pos :> Array
                            DefaultSemantic.Normals, norm :> Array
                            DefaultSemantic.Colors, col :> Array
                        ] |> SymDict.ofList,
                        [ 0 .. ((pos |> Array.length) - 1) ] |> List.toArray

                    let singleAttributes =
                        SymDict.empty

                    IndexedGeometry(IndexedGeometryMode.TriangleList, indices, indexedAttributes, singleAttributes)
                        |> Sg.ofIndexedGeometry

                Report.End() |> ignore
                res

            let sg = 
                aset {
                    let! floor = floor
                    yield floorISg floor
                }   |> Sg.set
                    |> Sg.effect [DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect; DefaultSurfaces.simpleLighting |> toEffect]
                    |> Sg.trafo ( scale |> Mod.map ( fun s -> Trafo3d.Scale s ) )
                    |> Sg.viewTrafo ( Context.cam |> Mod.map ( fun v -> CameraView.viewTrafo v ))
                    |> Sg.projTrafo ( Context.frustum |> Mod.map ( fun v -> Frustum.projTrafo v))
            
            Context.setRender sg
            Context.rc
    

    module XAMLHelpers =
        module Converter =
            let intOfString (v : obj) =
                Int32.Parse(string obj)

        let terrainGenerationLevelInput (win : MainWindow ) =
            let slider = win.terraingenerationlevelslider
            let button = win.terraingenerationbutton
            let res = int slider.Value |> Mod.init
            button.Click.Add ( fun v -> transact ( fun _ -> 0 |> Mod.change res ); transact ( fun _ -> int slider.Value |> Mod.change res ))
            res :> IMod<_>

        let terrainScaleInput ( win : MainWindow ) =
            let slider = win.terrainscaleslider
            let res = float slider.Value |> Mod.init
            slider.ValueChanged.Add ( fun v -> transact ( fun _ -> float v.NewValue |> Mod.change res ))
            res :> IMod<_>
