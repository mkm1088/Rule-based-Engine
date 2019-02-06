using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Globalization;
using CLIPSNET;
using System.Windows.Media.Imaging;
using System.ComponentModel;
using System.Windows.Documents;
using System.Windows.Navigation;
using System.Diagnostics;
using System.Windows.Input;
using System.Windows.Shapes;
using System.Data.Odbc;
using System.Data;
using System.Data.OleDb;

namespace ICTJobsAndCoursesRec
{
    public partial class ICTMainWindow : Window
    {
        private enum InterviewState { GREETING, INTERVIEW, CONCLUSION };

        private CLIPSNET.Environment clips = new CLIPSNET.Environment();
        private string lastAnswer = null;
        private string relationAsserted = null;
        private string competency = null;
        private string jobGroup = null;
        private List<string> variableAsserts = new List<string>();
        private List<string> priorAnswers = new List<string>();
        private List<string> competencyAnswers = new List<string>();
        private List<string> valueAnswers = new List<string>();
        private InterviewState interviewState;

        public ICTMainWindow()
        {
            InitializeComponent();
            prevButton.Tag = "Prev";
            clips.LoadFromResource("ICTJobsAndCoursesRec", "ICTJobsAndCoursesRec.ict.clp");
            clips.LoadFromResource("ICTJobsAndCoursesRec", "ICTJobsAndCoursesRec.ict_en.clp");
            clips.Reset();
        }

        private void OnLoad(object sender, RoutedEventArgs e)
        {
            progressPanel.Visibility = Visibility.Hidden;
            leftLabel.Visibility = Visibility.Hidden;
            rightLabel.Visibility = Visibility.Hidden;

            ProcessRules();
        }

        private void ProcessRules()
        {
            clips.Reset();
            foreach (string factString in variableAsserts)
            {
                string assertCommand = "(assert " + factString + ")";
                clips.Eval(assertCommand);
            }
            clips.Run();
            HandleResponse();
        }

        private void HandleResponse()
        {
            /*===========================*/
            /* Get the current UI state. */
            /*===========================*/
            string evalStr = "(find-fact ((?f UI-state)) TRUE)";
            FactAddressValue fv = (FactAddressValue)((MultifieldValue)clips.Eval(evalStr))[0];

            /*========================================*/
            /* Determine the Next/Prev button states. */
            /*========================================*/
            if (fv["state"].ToString().Equals("conclusion"))
            {
                interviewState = InterviewState.CONCLUSION;
                nextButton.Tag = "Restart";
                nextButton.Content = "Restart";
                prevButton.Visibility = Visibility.Hidden;
                scrollViewer.Visibility = Visibility.Visible;
                choicesPanel.Visibility = Visibility.Hidden;
                leftPanel.Visibility = Visibility.Hidden;
                rightPanel.Visibility = Visibility.Hidden;
            }
            else if (fv["state"].ToString().Equals("greeting"))
            {
                interviewState = InterviewState.GREETING;
                nextButton.Tag = "Next";
                nextButton.Content = "Next >";
                prevButton.Visibility = Visibility.Hidden;
                scrollViewer.Visibility = Visibility.Hidden;
                choicesPanel.Visibility = Visibility.Hidden;
                leftPanel.Visibility = Visibility.Hidden;
                rightPanel.Visibility = Visibility.Hidden;
            }
            else
            {
                interviewState = InterviewState.INTERVIEW;
                nextButton.Tag = "Next";
                nextButton.Content = "Next >";
                prevButton.Visibility = Visibility.Visible;
                scrollViewer.Visibility = Visibility.Hidden;
                choicesPanel.Visibility = Visibility.Visible;
                leftPanel.Visibility = Visibility.Visible;
                rightPanel.Visibility = Visibility.Visible;
            }
            leftLabel.Visibility = Visibility.Hidden;
            rightLabel.Visibility = Visibility.Hidden;

            // Show different image according to job group
            string currentPath = System.Environment.CurrentDirectory;
            string[] temp = currentPath.Split("\\".ToCharArray());
            currentPath = "";
            for (int i = 0; i < temp.Length - 2; i++)
            {
                currentPath += temp[i];
                currentPath += "\\";
            }

            /*=====================*/
            /* Set up the choices. */
            /*=====================*/
            resultsPanel.Children.Clear();
            choicesPanel.Children.Clear();
            leftPanel.Children.Clear();
            rightPanel.Children.Clear();

            /*===========================*/
            /* Set up the panel context. */
            /*===========================*/
            if (fv["state"].ToString().Equals("conclusion"))
            {
                MultifieldValue drmf = (MultifieldValue)fv["display-results"];
                Label linkLabel = null;
                Run linkText = null;
                Hyperlink link = null;

                // Read excel for link of Job & Course & Prodiver
                List<string> jobNameList = new List<string>();
                List<string> jobLinkList = new List<string>();
                List<string> courseNameList = new List<string>();
                List<string> courseLinkList = new List<string>();
                List<string> providerNameList = new List<string>();
                List<string> providerLinkList = new List<string>();
                DataTable dt = new DataTable();
                dt = LoadExcel(currentPath + "\\doc\\LinkData.xlsx", "Job");
                for (int i = 0; i < dt.Rows.Count; i++)
                {
                    jobNameList.Add(dt.Rows[i][0].ToString());
                    jobLinkList.Add(dt.Rows[i][1].ToString());
                }

                dt = LoadExcel(currentPath + "\\doc\\LinkData.xlsx", "Course");
                for (int i = 0; i < dt.Rows.Count; i++)
                {
                    courseNameList.Add(dt.Rows[i][0].ToString());
                    courseLinkList.Add(dt.Rows[i][1].ToString());
                }

                dt = LoadExcel(currentPath + "\\doc\\LinkData.xlsx", "Provider");
                for (int i = 0; i < dt.Rows.Count; i++)
                {
                    providerNameList.Add(dt.Rows[i][0].ToString());
                    providerLinkList.Add(dt.Rows[i][1].ToString());
                }

                for (int i = 0; i < drmf.Count; i++)
                {
                    LexemeValue dr = (LexemeValue)drmf[i];
                    string resultValue = dr.Value;
                    string[] resultValues = resultValue.Split(';');

                    // Job
                    StackPanel jPanel = new StackPanel();
                    jPanel.HorizontalAlignment = HorizontalAlignment.Left;
                    jPanel.VerticalAlignment = VerticalAlignment.Center;
                    jPanel.Orientation = Orientation.Horizontal;
                    jPanel.Height = 40;

                    Ellipse jEllipse = new Ellipse();
                    jEllipse.Width = 20;
                    jEllipse.Height = 20;
                    SolidColorBrush jSolidColorBrush = new SolidColorBrush();
                    jSolidColorBrush.Color = Color.FromArgb(255, 255, 255, 255);
                    jEllipse.Fill = jSolidColorBrush;
                    jPanel.Children.Add(jEllipse);

                    linkLabel = new Label();
                    linkText = new Run(" " + resultValues[0].ToString());
                    link = new Hyperlink(linkText);
                    link.NavigateUri = new Uri(jobLinkList[jobNameList.IndexOf(resultValues[0].ToString())]);
                    link.RequestNavigate += new RequestNavigateEventHandler(delegate (object sender, RequestNavigateEventArgs e) {
                        Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
                        e.Handled = true;
                    });
                    link.MouseEnter += new MouseEventHandler(OnLinkMouseEnter);
                    link.MouseLeave += new MouseEventHandler(OnLinkMouseLeave);
                    link.Foreground = System.Windows.Media.Brushes.White;
                    link.TextDecorations = null;
                    linkLabel.Content = link;
                    linkLabel.FontSize = 22;
                    linkLabel.Foreground = System.Windows.Media.Brushes.White;
                    jPanel.Children.Add(linkLabel);
                    resultsPanel.Children.Add(jPanel);

                    // Course1
                    StackPanel c1Panel = new StackPanel();
                    c1Panel.HorizontalAlignment = HorizontalAlignment.Left;
                    c1Panel.VerticalAlignment = VerticalAlignment.Center;
                    c1Panel.Orientation = Orientation.Horizontal;
                    c1Panel.Height = 30;
                    c1Panel.SetValue(Canvas.LeftProperty, 20d);

                    Label c1Label = new Label();
                    c1Label.Content = "    ";
                    c1Panel.Children.Add(c1Label);

                    Ellipse c1Ellipse = new Ellipse();
                    c1Ellipse.Width = 15;
                    c1Ellipse.Height = 15;
                    SolidColorBrush c1SolidColorBrush = new SolidColorBrush();
                    c1SolidColorBrush.Color = Color.FromArgb(255, 255, 255, 255);
                    c1Ellipse.Fill = c1SolidColorBrush;
                    c1Panel.Children.Add(c1Ellipse);

                    linkLabel = new Label();
                    linkText = new Run(resultValues[1].ToString());
                    link = new Hyperlink(linkText);
                    link.NavigateUri = new Uri(courseLinkList[courseNameList.IndexOf(resultValues[1].ToString())]);
                    link.RequestNavigate += new RequestNavigateEventHandler(delegate (object sender, RequestNavigateEventArgs e) {
                        Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
                        e.Handled = true;
                    });
                    link.MouseEnter += new MouseEventHandler(OnLinkMouseEnter);
                    link.MouseLeave += new MouseEventHandler(OnLinkMouseLeave);
                    link.Foreground = System.Windows.Media.Brushes.White;
                    link.TextDecorations = null;
                    linkLabel.Content = link;
                    linkLabel.FontSize = 18;
                    linkLabel.Foreground = System.Windows.Media.Brushes.White;
                    c1Panel.Children.Add(linkLabel);
                    resultsPanel.Children.Add(c1Panel);

                    // Provider1
                    StackPanel p1Panel = new StackPanel();
                    p1Panel.HorizontalAlignment = HorizontalAlignment.Left;
                    p1Panel.VerticalAlignment = VerticalAlignment.Center;
                    p1Panel.Orientation = Orientation.Horizontal;
                    p1Panel.Height = 30;
                    p1Panel.SetValue(Canvas.LeftProperty, 40d);

                    Label p1Label = new Label();
                    p1Label.Content = "        ";
                    p1Panel.Children.Add(p1Label);

                    Ellipse p1Ellipse = new Ellipse();
                    p1Ellipse.Width = 10;
                    p1Ellipse.Height = 10;
                    SolidColorBrush p1SolidColorBrush = new SolidColorBrush();
                    p1SolidColorBrush.Color = Color.FromArgb(255, 255, 255, 255);
                    p1Ellipse.Fill = p1SolidColorBrush;
                    p1Panel.Children.Add(p1Ellipse);

                    linkLabel = new Label();
                    linkText = new Run(resultValues[3].ToString());
                    link = new Hyperlink(linkText);
                    link.NavigateUri = new Uri(providerLinkList[providerNameList.IndexOf(resultValues[3].ToString())]);
                    link.RequestNavigate += new RequestNavigateEventHandler(delegate (object sender, RequestNavigateEventArgs e) {
                        Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
                        e.Handled = true;
                    });
                    link.MouseEnter += new MouseEventHandler(OnLinkMouseEnter);
                    link.MouseLeave += new MouseEventHandler(OnLinkMouseLeave);
                    link.Foreground = System.Windows.Media.Brushes.White;
                    link.TextDecorations = null;
                    linkLabel.Content = link;
                    linkLabel.FontSize = 14;
                    linkLabel.Foreground = System.Windows.Media.Brushes.White;
                    p1Panel.Children.Add(linkLabel);
                    resultsPanel.Children.Add(p1Panel);

                    // Course2
                    StackPanel c2Panel = new StackPanel();
                    c2Panel.HorizontalAlignment = HorizontalAlignment.Left;
                    c2Panel.VerticalAlignment = VerticalAlignment.Center;
                    c2Panel.Orientation = Orientation.Horizontal;
                    c2Panel.Height = 30;
                    c2Panel.SetValue(Canvas.LeftProperty, 20d);

                    Label c2Label = new Label();
                    c2Label.Content = "    ";
                    c2Panel.Children.Add(c2Label);

                    Ellipse c2Ellipse = new Ellipse();
                    c2Ellipse.Width = 15;
                    c2Ellipse.Height = 15;
                    SolidColorBrush c2SolidColorBrush = new SolidColorBrush();
                    c2SolidColorBrush.Color = Color.FromArgb(255, 255, 255, 255);
                    c2Ellipse.Fill = c2SolidColorBrush;
                    c2Panel.Children.Add(c2Ellipse);

                    linkLabel = new Label();
                    linkText = new Run(resultValues[2].ToString());
                    link = new Hyperlink(linkText);
                    link.NavigateUri = new Uri(courseLinkList[courseNameList.IndexOf(resultValues[2].ToString())]);
                    link.RequestNavigate += new RequestNavigateEventHandler(delegate (object sender, RequestNavigateEventArgs e) {
                        Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
                        e.Handled = true;
                    });
                    link.MouseEnter += new MouseEventHandler(OnLinkMouseEnter);
                    link.MouseLeave += new MouseEventHandler(OnLinkMouseLeave);
                    link.Foreground = System.Windows.Media.Brushes.White;
                    link.TextDecorations = null;
                    linkLabel.Content = link;
                    linkLabel.FontSize = 18;
                    linkLabel.Foreground = System.Windows.Media.Brushes.White;
                    c2Panel.Children.Add(linkLabel);
                    resultsPanel.Children.Add(c2Panel);

                    // Provider2
                    StackPanel p2Panel = new StackPanel();
                    p2Panel.HorizontalAlignment = HorizontalAlignment.Left;
                    p2Panel.VerticalAlignment = VerticalAlignment.Center;
                    p2Panel.Orientation = Orientation.Horizontal;
                    p2Panel.Height = 30;
                    p2Panel.SetValue(Canvas.LeftProperty, 40d);

                    Label p2Label = new Label();
                    p2Label.Content = "        ";
                    p2Panel.Children.Add(p2Label);

                    Ellipse p2Ellipse = new Ellipse();
                    p2Ellipse.Width = 10;
                    p2Ellipse.Height = 10;
                    SolidColorBrush p2SolidColorBrush = new SolidColorBrush();
                    p2SolidColorBrush.Color = Color.FromArgb(255, 255, 255, 255);
                    p2Ellipse.Fill = p2SolidColorBrush;
                    p2Panel.Children.Add(p2Ellipse);

                    linkLabel = new Label();
                    linkText = new Run(resultValues[4].ToString());
                    link = new Hyperlink(linkText);
                    link.NavigateUri = new Uri(providerLinkList[providerNameList.IndexOf(resultValues[4].ToString())]);
                    link.RequestNavigate += new RequestNavigateEventHandler(delegate (object sender, RequestNavigateEventArgs e) {
                        Process.Start(new ProcessStartInfo(e.Uri.AbsoluteUri));
                        e.Handled = true;
                    });
                    link.MouseEnter += new MouseEventHandler(OnLinkMouseEnter);
                    link.MouseLeave += new MouseEventHandler(OnLinkMouseLeave);
                    link.Foreground = System.Windows.Media.Brushes.White;
                    link.TextDecorations = null;
                    linkLabel.Content = link;
                    linkLabel.FontSize = 14;
                    linkLabel.Foreground = System.Windows.Media.Brushes.White;
                    p2Panel.Children.Add(linkLabel);
                    resultsPanel.Children.Add(p2Panel);
                }
                scrollViewer.HorizontalScrollBarVisibility = ScrollBarVisibility.Auto;
                scrollViewer.VerticalScrollBarVisibility = ScrollBarVisibility.Auto;
                scrollViewer.SetValue(Canvas.HeightProperty, 450d);
                resultsPanel.VerticalAlignment = VerticalAlignment.Top;

                Uri uri = null;
                if (jobGroup != null)
                {
                    uri = new Uri(currentPath + "\\image\\" + jobGroup + ".png", UriKind.RelativeOrAbsolute);
                }
                else
                {
                    uri = new Uri(currentPath + "\\image\\Path.png", UriKind.RelativeOrAbsolute);
                }

                BitmapImage bitmap = new BitmapImage(uri);
                pathImage.Source = bitmap;

                // Change window background
                if (jobGroup != null)
                {
                    string color = "";
                    switch (jobGroup)
                    {
                        case "JobGroupsBA":
                            color = "#FF20C2D7";
                            break;
                        case "JobGroupsCC":
                            color = "#FF0886B7";
                            break;
                        case "JobGroupsDDM":
                            color = "#FF27BBD5";
                            break;
                        case "JobGroupsINS":
                            color = "#FF007386";
                            break;
                        case "JobGroupsISM":
                            color = "#FF059DCC";
                            break;
                        case "JobGroupsITM":
                            color = "#FF008CAD";
                            break;
                        case "JobGroupsSSD":
                            color = "#FF00AB9B";
                            break;
                    }

                    prevButton.Background = nextButton.Background = this.Background = new SolidColorBrush((Color)ColorConverter.ConvertFromString(color));
                }
                else {
                    this.Background = new SolidColorBrush((Color)ColorConverter.ConvertFromString("#FF75B2B3"));
                }

                // Hidden progressBar
                progressPanel.Visibility = Visibility.Hidden;
            }
            else
            {
                MultifieldValue damf = (MultifieldValue)fv["display-answers"];
                MultifieldValue vamf = (MultifieldValue)fv["valid-answers"];

                string selected = fv["response"].ToString();
                RadioButton firstButton = null;

                Uri uri = null;
                BitmapImage bitmap = null;

                for (int i = 0; i < damf.Count; i++)
                {

                    LexemeValue da = (LexemeValue)damf[i];
                    LexemeValue va = (LexemeValue)vamf[i];
                    RadioButton rButton;
                    Button button;
                    string buttonName, buttonText, buttonAnswer;

                    buttonName = da.Value;
                    buttonText = buttonName.Substring(0, 1).ToUpperInvariant() + buttonName.Substring(1);
                    buttonAnswer = va.Value;

                    // Character
                    if (fv["relation-asserted"].ToString().Equals("mind-types") ||
                        fv["relation-asserted"].ToString().Equals("see-things") ||
                        fv["relation-asserted"].ToString().Equals("judge-things") ||
                        fv["relation-asserted"].ToString().Equals("act-towards-changes")) {
                        button = new Button();

                        // Button with image
                        uri = new Uri(currentPath + "\\image\\" + buttonAnswer + ".png", UriKind.RelativeOrAbsolute);
                        bitmap = new BitmapImage(uri);
                        Image cellImage = new Image();
                        cellImage.Source = bitmap;
                        button = new Button();
                        button.Content = cellImage;
                        button.Tag = buttonAnswer;
                        button.Width = 150;
                        button.Height = 150;
                        button.IsDefault = false;
                        button.Click += OnClickButton;

                        button.Visibility = Visibility.Visible;
                        button.Margin = new Thickness(5);
                        if (leftPanel.Children.Count == 0)
                        {
                            button.IsDefault = true;
                            leftPanel.Children.Add(button);
                            leftLabel.Content = buttonText;
                            leftLabel.Visibility = Visibility.Visible;
                        }
                        else {
                            rightPanel.Children.Add(button);
                            rightLabel.Content = buttonText;
                            rightLabel.Visibility = Visibility.Visible;
                        }
                        
                        button.SetValue(Canvas.LeftProperty, 10d);
                        button.SetValue(Canvas.TopProperty, 10d);
                        button.SetValue(Canvas.BackgroundProperty, null);
                    }
                    else
                    {
                        rButton = new RadioButton();
                        rButton.Content = buttonText;
                        if (((lastAnswer != null) && buttonAnswer.Equals(lastAnswer)) ||
                            ((lastAnswer == null) && buttonAnswer.Equals(selected)))
                        {
                            rButton.IsChecked = true;
                        }
                        else
                        {
                            rButton.IsChecked = false;
                        }

                        rButton.Tag = buttonAnswer;
                        rButton.Visibility = Visibility.Visible;
                        rButton.Margin = new Thickness(5);
                        choicesPanel.Children.Add(rButton);

                        if (firstButton == null)
                        {
                            firstButton = rButton;
                        }
                    }
                }

                if ((GetCheckedChoiceButton() == null) && (firstButton != null))
                {
                    firstButton.IsChecked = true;
                }

                relationAsserted = ((LexemeValue)fv["relation-asserted"]).Value;
                competency = fv["competency"].ToString();

                // Change background when click Prev
                if (fv["state"].ToString().Equals("greeting"))
                {
                    uri = new Uri(currentPath + "\\image\\Login.png", UriKind.RelativeOrAbsolute);
                }
                else {
                    uri = new Uri(currentPath + "\\image\\Path.png", UriKind.RelativeOrAbsolute);
                }
                bitmap = new BitmapImage(uri);
                pathImage.Source = bitmap;
                prevButton.Background = nextButton.Background = this.Background = new SolidColorBrush((Color)ColorConverter.ConvertFromString("#FF75B2B3"));

                // Update progressBar
                progressBar.Value = variableAsserts.Count * 100 / 13;
            }

            /*====================================*/
            /* Set the label to the display text. */
            /*====================================*/
            string messageString = ((StringValue)fv["display"]).Value;
            double theWidth = ComputeTextBoxWidth(messageString);
            messageTextBox.Width = theWidth;
            messageTextBox.MinWidth = theWidth;
            messageTextBox.Text = messageString;
        }

        private MultifieldValue GetNewResult(List<string> competencyAnswers, string v)
        {
            throw new NotImplementedException();
        }

        private void OnClickButton(object sender, RoutedEventArgs e)
        {
            Button button = sender as Button;

            if(button.Tag.Equals("Next"))
            {
                NextButtonAction();
            }
            else if (button.Tag.Equals("Restart"))
            {
                NextButtonAction();
            }
            else if (button.Tag.Equals("Prev"))
            {
                PrevButtonAction();
            }
            else if (button.Tag.ToString().Substring(0, 11).Equals("Personality"))
            {
                button.IsDefault = true;
                NextButtonAction();
            }
        }

        private void NextButtonAction()
        {
            string theString;
            string theAnswer;

            lastAnswer = null;

            switch (interviewState)
            {
                case InterviewState.GREETING:
                case InterviewState.INTERVIEW:
                    theAnswer = (string)GetCheckedChoiceButton().Tag;
                    theString = "(" + relationAsserted + " " + theAnswer + ")";
                    variableAsserts.Add(theString);
                    priorAnswers.Add(theAnswer);

                    if (competency != "none" && int.Parse(competency) > 0)
                    {
                        theString = "(competency" + " " + competency + ")";
                        variableAsserts.Add(theString);
                    }

                    if (relationAsserted == "job-groups") {
                        jobGroup = theAnswer;
                    }
                    break;

                case InterviewState.CONCLUSION:
                    variableAsserts.Clear();
                    priorAnswers.Clear();
                    progressPanel.Visibility = Visibility.Visible;
                    break;
            }

            ProcessRules();
        }

        private void PrevButtonAction()
        {
            if (priorAnswers.Count - 1 < 0) {
                return;
            }

            lastAnswer = priorAnswers.ElementAt(priorAnswers.Count - 1);
            variableAsserts.RemoveAt(variableAsserts.Count - 1);
            priorAnswers.RemoveAt(priorAnswers.Count - 1);

            leftLabel.Visibility = Visibility.Hidden;
            rightLabel.Visibility = Visibility.Hidden;

            ProcessRules();
        }

        private RadioButton GetCheckedChoiceButton()
        {
            foreach (UIElement element in choicesPanel.Children)
            {
                if (element is RadioButton) {
                    RadioButton control = (RadioButton)element;
                    if (control.IsChecked == true)
                    {
                        return control;
                    }
                }
            }

            foreach (UIElement element in leftPanel.Children)
            {
                if (element is Button) {
                    Button control = (Button)element;
                    if (control.IsDefault == true)
                    {
                        RadioButton rButton = new RadioButton();
                        rButton.IsChecked = true;
                        rButton.Tag = control.Tag;
                        return rButton;
                    }
                }
            }

            foreach (UIElement element in rightPanel.Children)
            {
                if (element is Button)
                {
                    Button control = (Button)element;
                    if (control.IsDefault == true)
                    {
                        RadioButton rButton = new RadioButton();
                        rButton.IsChecked = true;
                        rButton.Tag = control.Tag;
                        return rButton;
                    }
                }
            }

            return null;
        }

        public DataTable LoadExcel(string path, string parameter)
        {
            string strConn = "Provider=Microsoft.ACE.OLEDB.12.0;" + "Data Source=" + path + ";" + "Extended Properties=Excel 12.0;";
            string strExcel = "select * from ["+ parameter  + "$]";
            OleDbConnection ole = new OleDbConnection(strConn);
            ole.Open();                                                 
            DataTable schemaTable = new DataTable();
            OleDbDataAdapter odp = new OleDbDataAdapter(strExcel, strConn);
            odp.Fill(schemaTable);
            ole.Close();
            return schemaTable;
        }

        private void OnLinkMouseEnter(object sender, MouseEventArgs e)
        {
            Hyperlink link = (Hyperlink)e.OriginalSource;
            link.Foreground = System.Windows.Media.Brushes.Red;
        }
        private void OnLinkMouseLeave(object sender, MouseEventArgs e)
        {
            Hyperlink link = (Hyperlink)e.OriginalSource;
            link.Foreground = System.Windows.Media.Brushes.White;
        }

        private double ComputeTextBoxWidth(string theString)
        {
            FormattedText theText = new FormattedText(theString,
                                                      CultureInfo.CurrentUICulture,
                                                      FlowDirection.LeftToRight,
                                                      new Typeface(this.messageTextBox.FontFamily,
                                                                   this.messageTextBox.FontStyle,
                                                                   this.messageTextBox.FontWeight,
                                                                   this.messageTextBox.FontStretch),
                                                      this.messageTextBox.FontSize,
                                                      Brushes.Black);

            double availableWidth = this.Width - 30;
            theText.MaxTextWidth = availableWidth;

            double initialWidth = theText.WidthIncludingTrailingWhitespace;
            double initialHeight = theText.Height;
            int reductions = 0;
            if (initialWidth <= 12.0)
            {
                return initialWidth;
            }

            theText.MaxTextWidth = initialWidth - 12.0;

            while ((initialHeight >= theText.Height) && (theText.WidthIncludingTrailingWhitespace > 12.0))
            {
                reductions++;
                theText.MaxTextWidth -= 12.0;
            }

            double finalWidth;
            if (reductions == 0)
            {
                finalWidth = availableWidth;
            }
            else
            {
                finalWidth = initialWidth - (12.0 * reductions);
            }

            return finalWidth + 20;
        }
    }
}
