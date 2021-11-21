open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Linq
open System.Windows
open System.Windows.Controls
open System.Windows.Media // Brushes
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq
open FSharpx
open YY


type XamlMain = XAML<"WindowMain.xaml">

let TitleError = "Error - TestYY"

//===========================================================================
type WindowMain (w:XamlMain) as self =
    let rng = new System.Random()
    let WEIGHT_DIGITAL = 1.0
    let WEIGHT_ANALOG = 2.5
    let HEIGHT_DIGITAL = 40.0
    do
        w.Root.Loaded.Add(self.Loaded)
        w.buttonRegenerate.Click.Add(self.Regenerate)
    member this.Window = w.Root
    new () = WindowMain(XamlMain())
    member this.Loaded (args:RoutedEventArgs) =
        this.Regenerate(args)
    member this.Regenerate(args:RoutedEventArgs) =
        w.chart.LineGroups.Clear()

        let t2 = DateTime.Now
        let t1 = t2.Subtract(TimeSpan.FromSeconds(1800.0))
        w.chart.TimeBegin <- t1
        w.chart.TimeEnd <- t2
        let n = 3000;

        let xvalues = new List<int64 * bool>()
        let mutable is_valid = true
        let mutable is_valid_count = rng.Next(1, 30)
        for i=0 to n-1 do
            let x = (t2.Ticks-t1.Ticks) * (int64 i) / ((int64 n)-1L) + t1.Ticks
            // 1. x-values.
            xvalues.Add(x, is_valid)
            is_valid_count <- is_valid_count - 1
            if is_valid_count<=0 then
                is_valid_count <- rng.Next(1,30)
                is_valid <- not is_valid
        w.chart.XValues <- xvalues
        w.chart.TimeBegin <- new DateTime(fst (xvalues.First()))
        w.chart.TimeEnd <- new DateTime(fst (xvalues.Last()))

        // A few digital charts.
        let ndigital = rng.Next(2, 5)
        for k=1 to ndigital do
            let mutable level = false
            let mutable lrun = rng.Next(1, 20)
            let y = Array.create n 0.0
            for i=0 to n-1 do
                y.[i] <- if level then PlcChart.LOGICAL_ONE else PlcChart.LOGICAL_ZERO
                lrun <- lrun - 1
                if lrun<0 then
                    level <- not level
                    lrun <- rng.Next(1, 20)

            let l = new PlcLine(Name = sprintf "D%d" (rng.Next(0,33)), Points=(y :> IEnumerable<double>))
            let lg = new PlcLineGroup()
            lg.Unit <- "Digital"
            lg.Weight <- WEIGHT_DIGITAL
            lg.Height <- new Nullable<double>(HEIGHT_DIGITAL)
            lg.Lines.Add(l)
            w.chart.LineGroups.Add(lg)

        // Some sinus charts in one group.
        let color_brushes = [| Brushes.Black; Brushes.Gray; Brushes.Blue; Brushes.Red; Brushes.Yellow; Brushes.Orange; Brushes.Violet; Brushes.Cyan; Brushes.Green; Brushes.Pink; Brushes.Brown |]
        let color_offset = rng.Next(0, color_brushes.Length-1)
        let nsinus = rng.Next(2,4)
        let sinus_lg = new PlcLineGroup()
        sinus_lg.Unit <- "V"
        sinus_lg.Weight <- WEIGHT_ANALOG
        for k=1 to nsinus do
            let amplitude = rng.NextDouble() * 100.0 + 3.0
            let phase = rng.NextDouble() * 3.0
            let frequency = rng.NextDouble() * 0.1 + 0.01
            let y = Array.create n 0.0
            for i=0 to n-1 do
                y.[i] <- amplitude * Math.Sin (phase + (float i)*frequency)
            let l = new PlcLine(Name = sprintf "Sinus %d" (rng.Next(0,33)), Points=(y :> IEnumerable<double>), Pen = new Pen(color_brushes.[(color_offset + k) % color_brushes.Length], 1.0))
            sinus_lg.Lines.Add(l)
        w.chart.LineGroups.Add(sinus_lg)

        // One random chart
        do
            let amplitude = 100.0
            let mutable level = (rng.NextDouble()-0.5) * amplitude
            let mutable ldir = (rng.NextDouble()-0.5) * 0.1
            let mutable lrun = rng.Next(1, 20)
            let y = Array.create n 0.0
            for i=0 to n-1 do
                y.[i] <- level
                level <- level + ldir
                lrun <- lrun - 1
                if lrun<0 then
                    lrun <- rng.Next(1, 20)
                    ldir <- (rng.NextDouble()-0.5) * 0.1

            let l = new PlcLine(Name = sprintf "Random %d" (rng.Next(0,33)), Points=(y :> IEnumerable<double>))
            let lg = new PlcLineGroup()
            lg.Unit <- "A"
            lg.Weight <- WEIGHT_ANALOG
            lg.Lines.Add(l)
            w.chart.LineGroups.Add(lg)
        w.chart.Redraw()
        // w.chart.InvalidateVisual()
        // w.chart.InvalidateArrange()
        // w.chart.UpdateLayout()

//===========================================================================
let unhandled_exception_handler (args: System.Windows.Threading.DispatcherUnhandledExceptionEventArgs) =
    args.Handled <- true
    MessageBox.Show(args.Exception.Message, TitleError) |> ignore

//===========================================================================
[<EntryPoint>]
[<STAThread>]
let main argv = 
    try
        let app = new Application()
        app.DispatcherUnhandledException.Add(unhandled_exception_handler)
        let wm = new WindowMain ()
        app.Run(wm.Window) |> ignore
    with
        | ex -> MessageBox.Show("Error:" + ex.Message, "Error") |> ignore
    0
