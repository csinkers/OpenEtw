module CreateFuzzEtl.GuiUtil
open System.Windows

type StackPanel () =
    inherit Controls.StackPanel(); do ()
    member this.ChildList with set values = values |> Seq.iter (this.Children.Add >> ignore)

type Grid () =
    inherit Controls.Grid(); do ()
    member this.ColDefs with set xs = xs |> Seq.iter (this.ColumnDefinitions.Add)
    member this.RowDefs with set xs = xs |> Seq.iter (this.RowDefinitions.Add)
    member this.ChildList with set xs = xs |> Seq.iter (fun (e, (x,xs), (y,ys)) -> 
                           this.Children.Add(e) |> ignore
                           Grid.SetColumn(e, x)
                           Grid.SetColumnSpan(e, xs)
                           Grid.SetRow(e, y)
                           Grid.SetRowSpan(e, ys))

type TabControl() =
    inherit Controls.TabControl(); do ()
    member this.ItemList with set xs = xs |> Seq.iter (this.Items.Add >> ignore)

let inGrid x y e = (e :> UIElement, (x, 1), (y, 1))
let inGridSpanCol (x, xspan) y e = (e :> UIElement, (x, xspan), (y, 1))
let inGridSpanRow x (y, yspan) e = (e :> UIElement, (x, 1), (y, yspan))
let inGridSpanBoth x y e = (e :> UIElement, x, y)

