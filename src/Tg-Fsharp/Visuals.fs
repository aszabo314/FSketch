namespace Tg

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
            }

        let empty = 
            {
                Positions = Array.empty
                Normals   = Array.empty
                Colors    = Array.empty
            }

    open RenderInfo
        
    //The mapping function Floor -> IndexedGeometry.
    //it's a recursive function traversing the Floor tree structure and collecting all the 
    //vertex attributes along the way. I use mutating C# lists here for memory efficiency
    //reasons. 
    module Geometry =
        
        let vfp ( x : FloorPoint ) =
            V3d( x.Pos.X, x.Pos.Y, x.Height )

        let norms (v0:V3d) v1 v2 =
            let oriented (n:V3d) =
                if n.Dot( V3d.OOI ) > Math.PI/2.0 then V3d(n.X,n.Y,-n.Z) else n

            v0.Cross v1 |> oriented, v0.Cross v2 |> oriented

        let rec ofFloor ( corners : Fork<FloorPoint> ) ( cw : Orientation ) ( after : Option<Fork<Floor>> )
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
                    
                    ofFloor TL.Corners Clockwise        TL.After pos norm col
                    ofFloor TR.Corners Counterclockwise TR.After pos norm col
                    ofFloor BL.Corners Counterclockwise BL.After pos norm col
                    ofFloor BR.Corners Clockwise        BR.After pos norm col
        
    //This is SceneGraph visualizing a Floor.
    //It depends on the Floor and a number of parameters. 
    let ofFloor ( floor : IMod<Terrain.Floor * float * float > ) ( scale : IMod<float> ) ( height : IMod<float> ) 
                ( waterEnabled : IMod<bool> ) ( colorEnabled : IMod<bool> ) ( colors : IMod<C4f[]> ) ( colorranges : IMod<float[]> ) =
        
        let minHeight = floor |> Mod.map ( fun (_,x,_) -> x )
        let maxHeight = floor |> Mod.map ( fun (_,_,x) -> x )
        let floor = floor     |> Mod.map ( fun (f,_,_) -> f )

        let floorISg ( floor : Floor ) =
            Report.BeginTimed("Creating Visualization ")

            //using a mutable list here, which is filled by the traversal function through mutation. 
            //with this list, additions are very cheap, and we have a lot of those. compared to the default F# list,
            //this is about twice as fast and needs about 70% memory. need to think about a better functional list
            //here (concat lists, continuation lists ....)
            let poslist = List<V3d>()
            let normlist = List<V3d>()
            let collist = List<C4b>()

            do Geometry.ofFloor floor.Corners Clockwise floor.After poslist normlist collist

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

        let c =  colors |> Mod.map ( fun c -> c |> Array.map ( fun v -> v.ToV4d() ) )
        let mi = c |> Mod.force |> Array.length
        let minHeight = minHeight |> Mod.map2 ( fun w h -> if w then (if h < 0.0 then 0.0 else h) else h ) waterEnabled

        let colorShader = Shader.withColor colorEnabled minHeight maxHeight

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
