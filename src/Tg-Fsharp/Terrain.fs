namespace Tg

open System

open Aardvark.Base
open Aardvark.Base.Incremental

//This module generates a fractal terrain using the simple midpoint displacement algorithm.
//It's a simple approach, more sophisiticated algorithms should be used here.
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
        }
        
    // this attribute adds some suffix to the name of the module when compiling.
    // I do this so I can give the module and the type the same name.
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FloorPoint =
        let HalfwayTowards (other : FloorPoint) (self : FloorPoint)=
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

        //this is the recursive function that builds the tree structure representing a Floor.
        //Note that this is function is not pure. When called repeatedly, it will create a different Floor
        //each time because of the random value. 
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
                    
                        let topmiddle = topleft         |> FloorPoint.HalfwayTowards topright
                        let leftmiddle = topleft        |> FloorPoint.HalfwayTowards bottomleft
                        let bottommiddle = bottomleft   |> FloorPoint.HalfwayTowards bottomright
                        let rightmiddle = topright      |> FloorPoint.HalfwayTowards bottomright

                        //center is average of all four corners.
                        let oldCenterTLBR = topleft     |> FloorPoint.HalfwayTowards bottomright
                        let oldCenterTRBL = topright    |> FloorPoint.HalfwayTowards bottomleft
                        let oldCenter = oldCenterTLBR   |> FloorPoint.HalfwayTowards oldCenterTRBL

                        //center gets a random height. That's why its a fractal terrain.
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
                    TopLeft =   { Pos = V2d.OO; Height = 0.0 }
                    TopRight =  { Pos = V2d.IO; Height = 0.0 }
                    BotLeft =   { Pos = V2d.OI; Height = 0.0 }
                    BotRight =  { Pos = V2d.II; Height = 0.0 }
                }
            let res = continueForks start levels
            Report.End() |> ignore
            res

    //This is a Floor depending on a set of parameters.
    //Because the generation function is impure, it does not compose properly. It (by definition)
    //has a different result each call even though the parameters are the same. The Mod system
    //can not detect a change in such a case.
    //If I want to generate a new Floor every time the button is clicked, I have to
    //"fake" a change in parameters by setting some parameter to a different value and 
    //then back to the same value. 
    //The proper solution would have been to add a timestamp to each parameter value, so
    //multiple calls with the same parameters at different times would have triggered the
    //algorithm. 
    let withParams ( level : IMod<int> ) ( sigma : IMod<float> ) ( roughness : IMod<float> ) ( flatness : IMod<float> ) =
        let minHeight (floor : Floor) =
            let rec findMin (f : Floor) (cur : float) : float =
                let smallest =
                    min (min (min (min f.Corners.TopLeft.Height f.Corners.TopRight.Height) f.Corners.BotLeft.Height) f.Corners.BotRight.Height) cur
                match f.After with
                | None -> smallest
                | Some a ->
                    let tl = findMin a.TopLeft  smallest
                    let tr = findMin a.TopRight smallest
                    let bl = findMin a.BotLeft  smallest
                    let br = findMin a.BotRight smallest
                    min (min (min tl tr) bl) br
            findMin floor Double.MaxValue

        let maxHeight (floor : Floor) =
            let rec findMax (f : Floor) (cur : float) : float =
                let biggest =
                    max (max (max (max f.Corners.TopLeft.Height f.Corners.TopRight.Height) f.Corners.BotLeft.Height) f.Corners.BotRight.Height) cur
                match f.After with
                | None -> biggest
                | Some a ->
                    let tl = findMax a.TopLeft  biggest
                    let tr = findMax a.TopRight biggest
                    let bl = findMax a.BotLeft  biggest
                    let br = findMax a.BotRight biggest
                    max (max (max tl tr) bl) br
            findMax floor Double.MinValue

        adaptive {
            let! maxLv = level
            let! sigma = sigma
            let! roughness = roughness
            let! flatness = flatness
            let floor = Algorithm.floor maxLv sigma roughness flatness
            let valley = minHeight floor
            let peak = maxHeight floor
            return floor, valley, peak
        }
