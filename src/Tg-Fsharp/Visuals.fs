namespace Tg

module Visuals =
    
    open System
    open System.Collections.Generic

    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Rendering
    open Aardvark.Rendering.GL
    open Aardvark.Rendering.NanoVg

    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.Semantics

    open Aardvark.Application
    open Aardvark.Application.WPF   
    
    open Terrain

    module Shader = 
        open FShade
        open DefaultSurfaces

        // the types of uniforms can be unboxed like this.
        type UniformScope with
            member x.Color0 : V4d =   uniform?Color0
            member x.Color1 : V4d =   uniform?Color1
            member x.Color2 : V4d =   uniform?Color2
            member x.Color3 : V4d =   uniform?Color3
            member x.Color4 : V4d =   uniform?Color4
            member x.Color5 : V4d =   uniform?Color5
            member x.Color6 : V4d =   uniform?Color6
            member x.Color7 : V4d =   uniform?Color7
            member x.Range0 : float = uniform?Range0
            member x.Range1 : float = uniform?Range1
            member x.Range2 : float = uniform?Range2
            member x.Range3 : float = uniform?Range3
            member x.Range4 : float = uniform?Range4
            member x.Range5 : float = uniform?Range5
            member x.Range6 : float = uniform?Range6
            member x.Range7 : float = uniform?Range7

        // using this function to set all my uniforms, because I can't seem to use arrays.
        let setUniformColorsAndRanges (colors : IMod<V4d[]>) (ranges : IMod<float[]>) (maxIndex : int) (sg : ISg) : ISg =
            let mutable osg = sg
            for i in 0..maxIndex do
                osg <- osg |> Sg.uniform (sprintf "Color%A" i) (colors |> Mod.map ( fun arr -> arr.[i] ))
                           |> Sg.uniform (sprintf "Range%A" i) (ranges |> Mod.map ( fun arr -> arr.[i] ))
            osg
        
        // if water is enabled, every vertex with z-coordinate below 0 is rendered at 0
        // can not currently use the record copy syntax { v with pos = newpos } because of a bug (program will crash)
        let withWater ( water : IMod<bool> ) ( v : Vertex ) =
            vertex {
                let waterenabled = !!water
                if waterenabled then
                     let vz = if v.pos.Z < 0.0 then 0.0 else v.pos.Z
                     let newcol = if v.pos.Z < 0.0 then V4d(0.0,0.0,1.0,1.0) else v.c
                     let newpos = V4d(v.pos.X, v.pos.Y, vz, v.pos.W)
                     let newnorm = if v.pos.Z < 0.0 then V3d(0.0,0.0,1.0) else v.n
                     return 
                        {
                            pos =   newpos
                            wp =    v.wp
                            n =     newnorm
                            b =     v.b
                            t =     v.t
                            c =     newcol
                            tc =    v.tc
                        }
                else return   
                        {
                            pos =   v.pos
                            wp =    v.wp
                            n =     v.n
                            b =     v.b
                            t =     v.t
                            c =     v.c
                            tc =    v.tc
                        }
            }

        let withColor ( enabled : IMod<bool> ) ( minTerrainHeight : IMod<float> ) ( maxTerrainHeight : IMod<float> ) ( v : Vertex ) =
            vertex {
                let color0 = uniform.Color0
                let color1 = uniform.Color1
                let color2 = uniform.Color2
                let color3 = uniform.Color3
                let color4 = uniform.Color4
                let color5 = uniform.Color5
                let color6 = uniform.Color6
                let color7 = uniform.Color7
                let range0 = uniform.Range0
                let range1 = uniform.Range1
                let range2 = uniform.Range2
                let range3 = uniform.Range3
                let range4 = uniform.Range4
                let range5 = uniform.Range5
                let range6 = uniform.Range6
                let range7 = uniform.Range7
                let maxIndex = 8
                let enabled = !!enabled
                if enabled then
                    let smallest = !!minTerrainHeight
                    let biggest = !!maxTerrainHeight
                    let maxIdx = float maxIndex
                    let height = v.pos.Z
                    let normheight = ( height - smallest ) / ( biggest - smallest )
                    let idx = int (floor(normheight * maxIdx))
                    let oh = if      idx = 0 then range0
                             else if idx = 1 then range1
                             else if idx = 2 then range2
                             else if idx = 3 then range3
                             else if idx = 4 then range4
                             else if idx = 5 then range5
                             else if idx = 6 then range6
                             else if idx = 7 then range7
                             else range7
                    let nh = int (floor(oh * maxIdx))
                    let ocol = if      nh = 0 then color0
                               else if nh = 1 then color1
                               else if nh = 2 then color2
                               else if nh = 3 then color3
                               else if nh = 4 then color4
                               else if nh = 5 then color5
                               else if nh = 6 then color6
                               else if nh = 7 then color7
                               else color7
                    return   
                        {
                            pos =   v.pos
                            wp =    v.wp
                            n =     v.n
                            b =     v.b
                            t =     v.t
                            c =     ocol
                            tc =    v.tc
                        }
                else return   
                        {
                            pos =   v.pos
                            wp =    v.wp
                            n =     v.n
                            b =     v.b
                            t =     v.t
                            c =     v.c
                            tc =    v.tc
                        }
            }

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

            let mutable cam = 
                let initialCam = CameraView.LookAt (V3d(-30.0,-30.0,30.0), V3d(0.0,0.0,10.0))
                DefaultCameraController.control rc.Mouse rc.Keyboard rc.Time initialCam
                    
            let setRender (sg : ISg) = 
                rc.RenderTask <- 
                    oglapp.Runtime.CompileRender(rc.FramebufferSignature, BackendConfiguration.ManagedOptimized, sg) 
//                        |> DefaultOverlays.withStatistics

        //this is a RenderControl that depends on one Floor as its content
        let ofFloor ( floor : IMod<Terrain.Floor * float * float > ) ( scale : IMod<float> ) ( height : IMod<float> ) 
                    ( waterEnabled : IMod<bool> ) ( colors : IMod<C4f[]> ) ( colorranges : IMod<float[]> ) =
            
            let minHeight = floor |> Mod.map ( fun (_,x,_) -> x )
            let maxHeight = floor |> Mod.map ( fun (_,_,x) -> x )
            let floor = floor     |> Mod.map ( fun (f,_,_) -> f )

            let vfp ( x : FloorPoint ) =
                V3d( x.Pos.X, x.Pos.Y, x.Height )

            let norms (v0:V3d) v1 v2 =
                let oriented (n:V3d) =
                    if n.Dot( V3d.OOI ) > Math.PI/2.0 then V3d(n.X,n.Y,-n.Z) else n

                v0.Cross v1 |> oriented, v0.Cross v2 |> oriented

            let rec floorGeometry ( corners : Fork<FloorPoint> ) ( cw : Orientation ) ( after : Option<Fork<Floor>> )
                (pos:List<V3d>) (norm:List<V3d>) (col:List<C4b>) =
                match after with
                    //there are no successors, the result is a Sg made of the current geometry (list of single element)
                    | None ->                         
                        let topleftpoint = corners.TopLeft      |> vfp
                        let toprightpoint = corners.TopRight    |> vfp
                        let bottomleftpoint = corners.BotLeft   |> vfp
                        let bottomrightpoint = corners.BotRight |> vfp

                        //orientation determines where the diagonal is (has to point toward the middle).
                        //all vertices appear twice since they have different normals.
                        match cw with
                            | Clockwise ->
                                pos.Add topleftpoint; pos.Add toprightpoint; pos.Add bottomrightpoint; pos.Add topleftpoint; pos.Add bottomrightpoint; pos.Add bottomleftpoint
                                col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White

                                let v0 = (bottomrightpoint - topleftpoint).Normalized
                                let v1 = (toprightpoint    - topleftpoint).Normalized
                                let v2 = (bottomleftpoint  - topleftpoint).Normalized

                                let (n012,n023) = norms v0 v1 v2

                                norm.Add n012; norm.Add n012; norm.Add n012; norm.Add n023; norm.Add n023; norm.Add n023
                            | Counterclockwise ->
                                pos.Add toprightpoint; pos.Add  bottomrightpoint; pos.Add  bottomleftpoint; pos.Add  toprightpoint; pos.Add  bottomleftpoint; pos.Add  topleftpoint
                                col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White; col.Add C4b.White

                                let v0 = (bottomleftpoint  - toprightpoint).Normalized
                                let v1 = (bottomrightpoint - toprightpoint).Normalized
                                let v2 = (topleftpoint     - toprightpoint).Normalized

                                let (n012,n023) = norms v0 v1 v2

                                norm.Add n012; norm.Add n012; norm.Add n012; norm.Add n023; norm.Add n023; norm.Add n023

                    //there are successors, our result is the list of their results
                    | Some after ->                                     
                        let TL = after.TopLeft
                        let TR = after.TopRight
                        let BL = after.BotLeft
                        let BR = after.BotRight
                        
                        floorGeometry TL.Corners Clockwise        TL.After pos norm col
                        floorGeometry TR.Corners Counterclockwise TR.After pos norm col
                        floorGeometry BL.Corners Counterclockwise BL.After pos norm col
                        floorGeometry BR.Corners Clockwise        BR.After pos norm col

                        
            let floorISg ( floor : Floor ) =
                Report.BeginTimed("Creating Visualization ")

                //using a mutable list here, which is filled by the traversal function through mutation. 
                //with this list additions are very cheap, and we have a lot of those. compared to the default F# list,
                //this is about twice as fast and needs about 70% memory. need to think about a better functional list
                //here (concat lists, continuation lists ....)
                let poslist = List<V3d>()
                let normlist = List<V3d>()
                let collist = List<C4b>()

                floorGeometry floor.Corners Clockwise floor.After
                                poslist normlist collist

                let pos = poslist.ToArray()
                let norm = normlist.ToArray()
                let col = collist.ToArray()

                let res =
                    let (indexedAttributes, indices) = 
                        [
                            DefaultSemantic.Positions, pos :> Array
                            DefaultSemantic.Normals, norm :> Array
                            DefaultSemantic.Colors, col :> Array
                        ] |> SymDict.ofList,
                        let vc = pos |> Array.length
                        let indices = Array.init vc id

                        //patch count * vertices per patch * size of float * attributes of vertex * copies of each attribute in memory
                        Report.Line("{0} vertices, estimated memory: {1:0.000} MB", vc, (float vc * 6.0 * 4.0 * 3.0 * 3.0)/1024.0/1024.0)

                        indices
                        

                    let singleAttributes =
                        SymDict.empty

                    IndexedGeometry(IndexedGeometryMode.TriangleList, indices, indexedAttributes, singleAttributes)
                        |> Sg.ofIndexedGeometry

                Report.End() |> ignore
                res

            let sg = 
                let c =  colors |> Mod.map ( fun c -> c |> Array.map ( fun v -> v.ToV4d() ) )
                let mi = c |> Mod.force |> Array.length
                
                let minHeight = minHeight |> Mod.map2 ( fun w h -> if w then (if h < 0.0 then 0.0 else h) else h ) waterEnabled

                let colorShader = Shader.withColor (Mod.constant true) minHeight maxHeight

                aset {
                    let! floor = floor
                    yield floorISg floor
                }   |> Sg.set
                    |> Shader.setUniformColorsAndRanges c colorranges mi
                    |> Sg.effect [  
                                    colorShader                     |> toEffect
                                    Shader.withWater waterEnabled   |> toEffect
                                    DefaultSurfaces.trafo           |> toEffect 
                                    DefaultSurfaces.vertexColor     |> toEffect 
                                    DefaultSurfaces.simpleLighting  |> toEffect]
                    |> Sg.trafo ( scale |> Mod.map ( fun s -> Trafo3d.Scale s ) )
                    |> Sg.trafo ( height |> Mod.map ( fun s -> Trafo3d.Scale ( V3d(1.0, 1.0, s ) ) ))
                    |> Sg.viewTrafo ( Context.cam |> Mod.map ( fun v -> CameraView.viewTrafo v ))
                    |> Sg.projTrafo ( Context.frustum |> Mod.map ( fun v -> Frustum.projTrafo v))
            
            Context.setRender sg
            Context.rc
    

    module XAMLHelpers =
        module Converter =
            let intOfString (v : obj) =
                Int32.Parse(string obj)

        module Events =
            open System.Windows
            let modColorPicker (colorPicker:DropDownCustomColorPicker.CustomColorPicker) : IMod<C4f> =
                let c4fOfColor (c:Media.Color) : C4f = C4b(c.R,c.G,c.B,c.A).ToC4f()
                let res = colorPicker.SelectedColor |> c4fOfColor |> Mod.init
                colorPicker.add_SelectedColorChanged ( fun nv -> transact( fun _ -> c4fOfColor nv |> Mod.change res) )
                res :> IMod<_>

            let modSlider (slider:System.Windows.Controls.Slider) =
                let res = float slider.Value |> Mod.init
                slider.ValueChanged.Add ( fun v -> transact ( fun _ -> float v.NewValue |> Mod.change res ))
                res :> IMod<_>

            //why
            let private wtf (b : Nullable<bool>) = if not b.HasValue then false else b.Value
            
            let modCheckbox (checkbox:System.Windows.Controls.CheckBox) =
                let res = wtf checkbox.IsChecked |> Mod.init
                checkbox.Click.Add ( fun v -> transact ( fun _ -> Mod.change res <| wtf checkbox.IsChecked) )
                res :> IMod<_>

            // create a ModRef which is explicitly updated with the slider value each time a button is clicked
            // c is a converter function for the slider value (ex. the cast function to int called 'int')
            let sliderOnButton<'a> (c : float -> 'a) (button:System.Windows.Controls.Button) (slider:System.Windows.Controls.Slider) =
                //new ModRef (slider initial value)
                let res = c slider.Value |> Mod.init
                //button callback that updates the ModRef
                //since nothing happens when the value doesn't change (repeated button clicks), I do a dummy change before the actual change
                button.Click.Add ( fun v -> transact ( fun _ -> Mod.change res Unchecked.defaultof<'a> ); transact ( fun _ -> c slider.Value |> Mod.change res ))
                //expose the ModRef as IMod (read-only)
                res :> IMod<_>

            let floatSlider button slider = sliderOnButton float button slider
            let intSlider   button slider = sliderOnButton int   button slider

        let terrainGenerationLevelInput (win : MainWindow ) =
            let slider = win.terraingenerationlevelslider
            let button = win.terraingenerationbutton
            Events.intSlider button slider

        let terrainScaleInput ( win : MainWindow ) =
            win.terrainscaleslider |> Events.modSlider

        let terrainHeightInput ( win : MainWindow ) =
            win.terrainheightslider |> Events.modSlider
            

        let sigmaInput ( win : MainWindow ) =
            let slider = win.sigmaslider
            let button = win.terraingenerationbutton
            Events.floatSlider button slider

        let roughnessInput ( win : MainWindow ) =
            let slider = win.roughnessslider
            let button = win.terraingenerationbutton
            Events.sliderOnButton ( fun v -> (v - 0.5) * (-1.0) )  button slider

        let flatnessInput ( win : MainWindow ) =
            let slider = win.flatnessslider
            let button = win.terraingenerationbutton
            Events.floatSlider button slider

        let waterEnabledInput ( win : MainWindow ) =
            win.waterenabledcheckbox |> Events.modCheckbox

        let colorsAndRanges ( win : MainWindow ) : IMod<C4f[]> * IMod<float[]> =
            
            let (c0,r0) = win.colorpicker0 |> Events.modColorPicker, win.rangepicker0 |> Events.modSlider
            let (c1,r1) = win.colorpicker1 |> Events.modColorPicker, win.rangepicker1 |> Events.modSlider
            let (c2,r2) = win.colorpicker2 |> Events.modColorPicker, win.rangepicker2 |> Events.modSlider
            let (c3,r3) = win.colorpicker3 |> Events.modColorPicker, win.rangepicker3 |> Events.modSlider
            let (c4,r4) = win.colorpicker4 |> Events.modColorPicker, win.rangepicker4 |> Events.modSlider
            let (c5,r5) = win.colorpicker5 |> Events.modColorPicker, win.rangepicker5 |> Events.modSlider
            let (c6,r6) = win.colorpicker6 |> Events.modColorPicker, win.rangepicker6 |> Events.modSlider
            let (c7,r7) = win.colorpicker7 |> Events.modColorPicker, win.rangepicker7 |> Events.modSlider

            let cs = adaptive {
                let! c0 = c0
                let! c1 = c1
                let! c2 = c2
                let! c3 = c3
                let! c4 = c4
                let! c5 = c5
                let! c6 = c6
                let! c7 = c7
                return 
                    [| 
                        c0
                        c1
                        c2
                        c3
                        c4
                        c5
                        c6
                        c7
                    |]
            }

            let rs = adaptive {
                let! r0 = r0
                let! r1 = r1
                let! r2 = r2
                let! r3 = r3
                let! r4 = r4
                let! r5 = r5
                let! r6 = r6
                let! r7 = r7
                return 
                    [| 
                        r0
                        r1
                        r2
                        r3
                        r4
                        r5
                        r6
                        r7
                    |]
            }

            cs,rs

        //using an explicit callback here instead of the WPF marking callback because its shorter to write.
        let displayLabel ( msg : IMod<string> ) ( lab : Windows.Controls.Label ) =
            msg |> Mod.unsafeRegisterCallbackKeepDisposable        
                ( fun ns -> lab.Content <- ns) 
