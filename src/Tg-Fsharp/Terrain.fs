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
        member self.XFullwayTowards (other : FloorPoint) =
            let x = other.Pos.X
            let y = self.Pos.Y
            { other with Pos = V2d(x,y) }
        member self.YFullwayTowards (other : FloorPoint) =
            let x = self.Pos.X
            let y = other.Pos.Y
            { other with Pos = V2d(x,y) }

    type Floor = 
        {
            Min     : FloorPoint
            Max     : FloorPoint
            After   : Option<Fork<Floor>>
        }

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

        let rec continueForks ( min : FloorPoint ) ( max : FloorPoint ) ( level : int ) : Floor =
            match level with
                | 0 -> { Min = min; Max = max; After = None}
                | _ -> 
                    let nextlevel = level-1

                    let topleft = min
                    let topright = min.XFullwayTowards max
                    let bottomleft = min.YFullwayTowards max
                    let bottomright = max
                    
                    let topmiddle = topleft.HalfwayTowards topright
                    let leftmiddle = topleft.HalfwayTowards bottomleft
                    let bottommiddle = bottomleft.HalfwayTowards bottomright
                    let rightmiddle = topright.HalfwayTowards bottomright

                    //center has a random height. That's why its a fractal terrain.
                    let random = 0.0 //todo
                    let oldCenter = topleft.HalfwayTowards bottomright
                    let center = { oldCenter with Height = oldCenter.Height + random }

                    {
                        Min = min
                        Max = max
                        After = 
                            Some {      //FAIL - floor must have all 4 corners
                                TopLeft = continueForks topleft center nextlevel
                                TopRight= continueForks topmiddle rightmiddle nextlevel
                                BotLeft = continueForks leftmiddle bottommiddle nextlevel
                                BotRight= continueForks center bottomright nextlevel
                            }
                    }

        let floor ( levels : int ) =
            continueForks { Pos = V2d.OO; Height = 0.0 } { Pos = V2d.II; Height = 1.0 } levels

    let ofLevel ( level : IMod<int> ) =
        
        adaptive {
            let! maxLv = level
            return Algorithm.floor maxLv
        }

