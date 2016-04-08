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

        let gaussian mu sigma =
            let u1 = Context.random.NextDouble()
            let u2 = Context.random.NextDouble()
            let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) *
                                    Math.Sin(2.0 * Math.PI * u2)
            mu + sigma * randStdNormal

        // A weird random number generator optimized for this terrain generation.
        // Found by experimentation.
        let funnyGaussian level maxlevel roughness flatness sigma =
            let weightedSigma = pow sigma 2.0 / pow 2.0 (level * roughness)
            let result = gaussian 0.0 weightedSigma
            if (maxlevel-level) < float maxlevel * (2.0/3.0) then
                 result / pow flatness 1.1
            else result

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

        let floor ( levels : int ) ( sigma : float ) ( roughness : float ) ( flatness : float ) =
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

                        let random = Rng.funnyGaussian (float level) (float levels) roughness flatness sigma
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

    let withParams ( level : IMod<int> ) ( sigma : IMod<float> ) ( roughness : IMod<float> ) ( flatness : IMod<float> ) =
        adaptive {
            let! maxLv = level
            let! sigma = sigma
            let! roughness = roughness
            let! flatness = flatness
            return Algorithm.floor maxLv sigma roughness flatness
        }

