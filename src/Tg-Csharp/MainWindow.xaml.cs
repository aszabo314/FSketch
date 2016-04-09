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
        }
        
        public ContentControl renderingcontrol => (ContentControl)this.FindName("renderingGoesHere");
        public Slider terraingenerationlevelslider => (Slider)this.FindName("terrainGenerationLevel");
        public Slider terrainscaleslider => (Slider)this.FindName("TerrainScale");
        public Slider terrainheightslider => (Slider)this.FindName("TerrainHeight");
        public Slider sigmaslider => (Slider)this.FindName("SigmaSlider");
        public Slider roughnessslider => (Slider)this.FindName("RoughnessSlider");
        public Slider flatnessslider => (Slider)this.FindName("FlatnessSlider");
        public CheckBox waterenabledcheckbox => (CheckBox)this.FindName("waterEnabledCheckbox");
        public Button terraingenerationbutton => (Button)this.FindName("terrainGenerationButton");
    }
}
