module CreateFuzzEtl.Gui
open System.Windows
open System.Windows.Controls
open CreateFuzzEtl.GuiUtil

type FuzzerGui<'a> (updateFunc, initialState) =
    let xmlTextBox = new TextBox(AcceptsReturn = true)
    let parameterPanel = new StackPanel()
    let scrollViewer = new ScrollViewer(Content = parameterPanel)
    let generateButton = new Button(Content = "Process")

    let mutable state = initialState
    
    let window =
        let grid = 
            let cols = 
                [
                    new ColumnDefinition(Width = new GridLength(25.0, GridUnitType.Star))
                    new ColumnDefinition(Width = new GridLength(25.0, GridUnitType.Star))
                    new ColumnDefinition(Width = new GridLength(50.0, GridUnitType.Star))
                ]
            let rows =
                [
                    new RowDefinition(Height = new GridLength(40.0, GridUnitType.Pixel)) // Label
                    new RowDefinition(Height = new GridLength(100.0, GridUnitType.Star)) // Main controls
                ]

            new Grid(
                ColDefs = cols, 
                RowDefs = rows, 
                ChildList = 
                    [
                        inGrid 0 0 (new Label(Content = "Params"))
                        inGrid 1 0 generateButton
                        inGrid 2 0 (new Label(Content = "XML"))
                        inGridSpanCol (0,2) 1 scrollViewer
                        inGrid 2 1 xmlTextBox
                    ])

        new Window(Title = "ETL Fuzzer", Width = 1024.0, Height = 600.0, Content = grid)

    do
        generateButton.Click |> Event.add (fun e -> xmlTextBox.Text <- updateFunc state)

    member x.Window with get() = window
    member x.AddParam(name:string, getter : 'a -> float, setter : 'a -> float -> 'a, min, max) =
        let label = new Label(Content = name)
        let slider = 
            new Slider(
                Minimum = min, 
                Maximum = max, 
                Value = getter state, 
                TickFrequency = 1.0, 
                IsSnapToTickEnabled = true)

        let textBox = new Label(Content = string (getter state))

        slider.ValueChanged |> Event.add (fun e -> 
            textBox.Content <- string e.NewValue
            state <- setter state e.NewValue
        )

        let horzGrid = 
                let cols = 
                    [
                        new ColumnDefinition(Width = new GridLength(60.0, GridUnitType.Pixel))
                        new ColumnDefinition(Width = new GridLength(100.0, GridUnitType.Star))
                        new ColumnDefinition(Width = new GridLength(60.0, GridUnitType.Pixel))
                    ]
                let rows =
                    [
                        new RowDefinition(Height = new GridLength(100.0, GridUnitType.Star))
                    ]

                new Grid(
                    ColDefs = cols, 
                    RowDefs = rows, 
                    ChildList = 
                        [
                            inGrid 0 0 label
                            inGrid 1 0 slider
                            inGrid 2 0 textBox
                        ])

        parameterPanel.Children.Add horzGrid |> ignore
