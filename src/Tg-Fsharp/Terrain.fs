namespace Tg

open System

open Aardvark.Base
open Aardvark.Base.Incremental

module Terrain =

    type Fork<'a> =
        {
            TopLeft : 'a
            TopRight: 'a
            BotLeft : 'a
            BotRight: 'a
        }

    type FloorPoint =
        {
            Pos     : V2d
            Height  : float
        } with
        member self.HalfwayTowards (other : FloorPoint) =
            let x = self.Pos.X + (other.Pos.X - self.Pos.X) / 2.0
            let y = self.Pos.Y + (other.Pos.Y - self.Pos.Y) / 2.0
            let h = self.Height + (other.Height - self.Height) / 2.0
            {
                Pos    = V2d(x,y)
                Height = h
            }

    type Floor = 
        {
            Corners : Fork<FloorPoint>
            After   : Option<Fork<Floor>>
        }

    module Rng =
        module Context =
            let random = new Random()

        let random min max =
            let res = Context.random.NextDouble()
            let range = max - min
            min + res * range

    module Algorithm =
        
//        2D terrain plan:
//
//        topleft   --   topmiddle   --   topright
//
//        |		TL		    | 		TR		|
//        |				    |				|
//                         
//        leftmiddle  --  center   --  rightmiddle
//                         
//        |		BL		    |		BR		|
//        | 		        |               |					
//
//        bottomleft - bottommiddle -- bottomright

        let rec continueForks ( corners : Fork<FloorPoint> ) ( level : int ) : Floor =
            match level with
                | 0 -> { Corners = corners; After = None}
                | _ -> 
                    let nextlevel = level-1

                    let topleft = corners.TopLeft
                    let topright = corners.TopRight
                    let bottomleft = corners.BotLeft
                    let bottomright = corners.BotRight
                    
                    let topmiddle = topleft.HalfwayTowards topright
                    let leftmiddle = topleft.HalfwayTowards bottomleft
                    let bottommiddle = bottomleft.HalfwayTowards bottomright
                    let rightmiddle = topright.HalfwayTowards bottomright

                    //center is average of all four corners.
                    let oldCenterTLBR = topleft.HalfwayTowards bottomright
                    let oldCenterTRBL = topright.HalfwayTowards bottomleft
                    let oldCenter = oldCenterTLBR.HalfwayTowards oldCenterTRBL

                    //center has a random height. That's why its a fractal terrain.
                    let random = Rng.random -0.1 0.1
                    let center = { oldCenter with Height = oldCenter.Height + random }

                    {
                        Corners = corners
                        After   = 
                            Some {      
                                TopLeft = continueForks { TopLeft = topleft; TopRight = topmiddle; BotLeft = leftmiddle; BotRight = center } nextlevel
                                TopRight= continueForks { TopLeft = topmiddle; TopRight = topright; BotLeft = center; BotRight = rightmiddle } nextlevel
                                BotLeft = continueForks { TopLeft = leftmiddle; TopRight = center; BotLeft = bottomleft; BotRight = bottommiddle } nextlevel
                                BotRight= continueForks { TopLeft = center; TopRight = rightmiddle; BotLeft = bottommiddle; BotRight = bottomright } nextlevel
                            }
                    }

        let floor ( levels : int ) =
            Report.BeginTimed("Terrain generation for {0} lvls", levels)
            let start = 
                {
                    TopLeft = { Pos = V2d.OO; Height = 0.0 }
                    TopRight = { Pos = V2d.IO; Height = 0.0 }
                    BotLeft = { Pos = V2d.OI; Height = 0.0 }
                    BotRight = { Pos = V2d.II; Height = 0.0 }
                }
            let res = continueForks start levels
            Report.End() |> ignore
            res

    let ofLevel ( level : IMod<int> ) =
        adaptive {
            let! maxLv = level
            return Algorithm.floor maxLv
        }

