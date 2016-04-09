using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using DropDownCustomColorPicker;

namespace Tg
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            colorpicker0.SelectedColor = Color.FromArgb(255, 252, 230, 81 );
            colorpicker1.SelectedColor = Color.FromArgb(255, 212, 187, 76 );
            colorpicker2.SelectedColor = Color.FromArgb(255, 181, 170, 123);
            colorpicker3.SelectedColor = Color.FromArgb(255, 118, 219, 18 );
            colorpicker4.SelectedColor = Color.FromArgb(255, 133, 237, 28 );
            colorpicker5.SelectedColor = Color.FromArgb(255, 129, 201, 142);
            colorpicker6.SelectedColor = Color.FromArgb(255, 154, 181, 160);
            colorpicker7.SelectedColor = Color.FromArgb(255, 242, 244, 245);

        }
        
        public ContentControl renderingcontrol => (ContentControl)this.FindName("renderingGoesHere");
        public Slider terraingenerationlevelslider => (Slider)this.FindName("terrainGenerationLevel");
        public Slider terrainscaleslider => (Slider)this.FindName("TerrainScale");
        public Slider terrainheightslider => (Slider)this.FindName("TerrainHeight");
        public Slider sigmaslider => (Slider)this.FindName("SigmaSlider");
        public Slider roughnessslider => (Slider)this.FindName("RoughnessSlider");
        public Slider flatnessslider => (Slider)this.FindName("FlatnessSlider");
        public CheckBox waterenabledcheckbox => (CheckBox)this.FindName("waterEnabledCheckbox");
        public CheckBox colorenabledcheckbox => (CheckBox)this.FindName("colorsEnabledCheckbox");
        public Button terraingenerationbutton => (Button)this.FindName("terrainGenerationButton");
        public Label camerapositionlabel => (Label)this.FindName("CameraPosition");
        public Label maxheightlabel => (Label)this.FindName("PeakLabel");
        public Label minheightlabel => (Label)this.FindName("ValleyLabel");

        //Color Picker by Razan Paul at: http://www.codeproject.com/Articles/42849/Making-a-Drop-Down-Style-Custom-Color-Picker-in-WP
        public CustomColorPicker colorpicker0 => (CustomColorPicker)this.FindName("ColorPicker0");
        public CustomColorPicker colorpicker1 => (CustomColorPicker)this.FindName("ColorPicker1");
        public CustomColorPicker colorpicker2 => (CustomColorPicker)this.FindName("ColorPicker2");
        public CustomColorPicker colorpicker3 => (CustomColorPicker)this.FindName("ColorPicker3");
        public CustomColorPicker colorpicker4 => (CustomColorPicker)this.FindName("ColorPicker4");
        public CustomColorPicker colorpicker5 => (CustomColorPicker)this.FindName("ColorPicker5");
        public CustomColorPicker colorpicker6 => (CustomColorPicker)this.FindName("ColorPicker6");
        public CustomColorPicker colorpicker7 => (CustomColorPicker)this.FindName("ColorPicker7");

        public Slider rangepicker0 => (Slider)this.FindName("RangePicker0");
        public Slider rangepicker1 => (Slider)this.FindName("RangePicker1");
        public Slider rangepicker2 => (Slider)this.FindName("RangePicker2");
        public Slider rangepicker3 => (Slider)this.FindName("RangePicker3");
        public Slider rangepicker4 => (Slider)this.FindName("RangePicker4");
        public Slider rangepicker5 => (Slider)this.FindName("RangePicker5");
        public Slider rangepicker6 => (Slider)this.FindName("RangePicker6");
        public Slider rangepicker7 => (Slider)this.FindName("RangePicker7");
        
    }
}
