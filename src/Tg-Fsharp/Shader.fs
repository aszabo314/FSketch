namespace Tg

open System

open Aardvark.Base
open Aardvark.Base.Incremental

open FShade

open Aardvark.Rendering
open Aardvark.Rendering.GL

open Aardvark.SceneGraph
open Aardvark.SceneGraph.Semantics

//This module contains Shader programs used to display a Floor. The shaders are written and compiled using FShade, which
//allows to compose shaders in sequence. I use this to define several effects and apply them in turn: cut off
//every vertex below 0 if water is enabled, and color each vertex based on its height and a color mapping. 
//It is also possible to use Mods within shaders to make them adaptive, and to define adaptive
//shader Uniforms (I use both in this module).
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

    // using this function to set all my uniforms, because I can't use arrays (OpenGL only likes fixed size arrays, like matrices)
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

    // if color is enabled, every vertex receives a color from a color map (linear interpolation)
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
                let height = v.pos.Z
                let x = ( height - smallest ) / ( biggest - smallest )
                let ocol =  if x    <   range0 then color0
                            else if x    <   range1 then let y = ( x - range0 ) / ( range1 - range0 ) 
                                                         ( color1 * y ) + ( color0 * ( 1.0 - y ) )
                            else if x    <   range2 then let y = ( x - range1 ) / ( range2 - range1 ) 
                                                         ( color2 * y ) + ( color1 * ( 1.0 - y ))
                            else if x    <   range3 then let y = ( x - range2 ) / ( range3 - range2 ) 
                                                         ( color3 * y ) + ( color2 * ( 1.0 - y ))
                            else if x    <   range4 then let y = ( x - range3 ) / ( range4 - range3 )
                                                         ( color4 * y ) + ( color3 * ( 1.0 - y ))
                            else if x    <   range5 then let y = ( x - range4 ) / ( range5 - range4 )
                                                         ( color5 * y ) + ( color4 * ( 1.0 - y ))
                            else if x    <   range6 then let y = ( x - range5 ) / ( range6 - range5 )
                                                         ( color6 * y ) + ( color5 * ( 1.0 - y ))
                            else if x    <   range7 then let y = ( x - range6 ) / ( range7 - range6 )
                                                         ( color7 * y ) + ( color6 * ( 1.0 - y ))
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
