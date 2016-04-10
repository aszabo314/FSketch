namespace Tg

open System

open Aardvark.Base
open Aardvark.Base.Incremental

open Aardvark.Application
open Aardvark.Application.WPF

open Aardvark.SceneGraph

//This module contains some helper functions that map the values of certain WPF controls to IMods.
//That can be done by initializing a ModRef and setting callbacks on ValueChange events (or ButtonClick events or other events)
//to update the value of the ModRef. The ModRef is then exposed as an immutable IMod to the outside.
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
    
    let colorEnabledInput ( win : MainWindow ) =
        win.colorenabledcheckbox |> Events.modCheckbox
    
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
        let initialCam = CameraView.LookAt (V3d(-30.0,-30.0,30.0), V3d(0.0,0.0,10.0))
        DefaultCameraController.control rc.Mouse rc.Keyboard rc.Time initialCam
                
    let setRender (sg : ISg) = 
        rc.RenderTask <- 
            RenderTask.ofList[
                oglapp.Runtime.CompileClear(rc.FramebufferSignature, Mod.constant <| C4b(174,234,255).ToC4f() )
                oglapp.Runtime.CompileRender(rc.FramebufferSignature, BackendConfiguration.ManagedOptimized, sg) 
                ]



