// Trivial change 2
namespace YY
    open System
    open System.Collections.Generic
    open System.Windows
    open System.Linq
    open System.Windows.Input
    open System.Windows.Media

    // TODO: cursor?
    type public PlcLine() =
        member val public Name = "" with get, set
        member val public Pen = new Pen(Brushes.Black, 1.0) with get, set
        member val public Points:IEnumerable<double> = ([| |] :> IEnumerable<double>) with get, set

    type public PlcLineGroup() =
        member val public Unit = "" with get, set
        member val public Weight = 1.0 with get, set
        /// Override Weight when specified.
        member val public Height = new System.Nullable<float>() with get, set
        member val public Lines = new List<PlcLine>() with get, set
        member val public FormatString = "F1" with get, set
        member internal this.ActualHeight (pixels_per_weight:float) = if this.Height.HasValue then this.Height.Value else pixels_per_weight * this.Weight
        member val internal IsDigital = false with get, set

    type internal PlcLayout() =
        member val TimeBegin_Text = (null :> FormattedText) with get, set
        member val TimeBegin_Pos = new Point() with get, set
        member val TimeEnd_Text = (null :> FormattedText) with get, set
        member val TimeEnd_Pos = new Point() with get, set
        member val Legends_Text = new List<FormattedText>()
        member val Legends_Pos = new List<Point>()
        member val GroupBoxes = new List<Rect>()
        /// nsteps, stepsize, min, max, [| text, position |] 
        member val GroupYScales = new List<int * double * double * double * List<FormattedText * Point> >()
        member val Units = new List<FormattedText * Point>()
        member val HorizontalDivision = new List<FormattedText * Point>()
        member val XScale = (1, 1L, 0L, 1L) with get, set

    type public PlcChart() as self = 
        inherit FrameworkElement()
        let TEXT_HEIGHT = 30
        let GROUPBOX_THICKNESS = 1.0
        let GRID_THICKNESS = 0.5
        let MARGIN_LEFT, MARGIN_RIGHT = 5.0, 5.0
        let MARGIN_TOP, MARGIN_BOTTOM = 5.0, 5.0
        let GROUP_GROUP_SPACE = GROUPBOX_THICKNESS / 2.0
        let GROUP_BOTTOM_SPACE = 2.0
        let TEXT_TEXT_SPACE = 2.0
        let GROUP_LEGEND_SPACE = 2.0
        let CROSSHAIR_WIDTH = 10.0
        let VISUAL_INDEX_CHART = 0
        let VISUAL_INDEX_OVERLAY = 1
        let _format_text (text:string) (brush:Brush) = new FormattedText(text, System.Globalization.CultureInfo.CurrentCulture, FlowDirection.LeftToRight, self.Typeface, self.FontSize, brush)
        let _factors_autodetect = [| 0.1; 0.25; 0.5; 1.0; 2.5; 5.0; 10.0; 25.0; 50.0; 100.0 |]
        // Return value: nsteps, step_size, min_value, max_value
        let _autodetect_y_step (min_val:float) (max_val:double) (target_nsteps:float) =
            let diff_log10 = int (Math.Log10((max_val - min_val) / target_nsteps)) // scale factor
            let base_unit = Math.Pow(10.0, float diff_log10)
            let mutable best_step = None // (float, float, int)
            for f in _factors_autodetect do
                let step = base_unit * f
                let min_value_steps = int (Math.Floor (min_val / step))
                let max_value_steps = int (Math.Ceiling(max_val / step))
                let nsteps = max_value_steps - min_value_steps
                let min_value = step * (float min_value_steps)
                let max_value = step * (float max_value_steps)
                let step_penalty = if (float nsteps) > target_nsteps then (float nsteps) / target_nsteps else target_nsteps / (float nsteps)
                let trial_step = Some (step, step_penalty, nsteps)
                best_step <- match best_step with
                                | Some(_, best_penalty, _) -> if step_penalty<best_penalty then trial_step else best_step
                                | None -> trial_step
            match best_step with
            | Some (step, _, nsteps) -> nsteps, step, step * Math.Floor(min_val / step), step * Math.Ceiling(max_val / step)
            | None -> 2, 1.0, 0.0, 1.0

        let _factors_timesteps = [| 1;3;5;10;15;30;60;120;300;600;1800;3600;7200;14400;21600;43200;86400;172800;259200;604800 |] |> Array.map (fun s -> TimeSpan.FromSeconds(float s))
        // Timespan times.
        let _autodetect_time_step (min_val:int64) (max_val:int64) (target_steps:float) =
            let match_of_step (st:TimeSpan) = 
                let rdiff = (float (max_val - min_val)) / (float st.Ticks) / target_steps
                if rdiff<1.0 then 1.0/rdiff else rdiff
            let step = Array.minBy match_of_step _factors_timesteps
            let n0, n1 = (min_val + step.Ticks - 1L) / step.Ticks, max_val / step.Ticks
            (int (n1-n0), step.Ticks, n0*step.Ticks, n1*step.Ticks)

        // TODO: translations!
        let _timespan_PartNames =  [| (24*3600, "päev", "päeva"); (3600, "tund", "tundi"); (60, "minut", "minutit"); (1, "sekund", "sekundit"); |]
        let _name_of_timespan (seconds:int) =
            let mutable todo = seconds
            let sb = new System.Text.StringBuilder()
            for (unit_size, name_singular, name_plural) in _timespan_PartNames do
                let r = todo / unit_size
                todo <- todo - r * unit_size
                match r with
                | 0-> ()
                | 1 ->  if sb.Length>0 then (sb.Append(' ') |> ignore)
                        sb.Append("1 " + name_singular) |> ignore
                | n ->  if sb.Length>0 then (sb.Append(' ') |> ignore)
                        sb.Append(r.ToString() + " " + name_plural) |> ignore
            sb.ToString()
        let _create_layout () =
            let height, width = self.ActualHeight, self.ActualWidth
            let layout = new PlcLayout()

            layout.TimeBegin_Text <-_format_text (self.TimeBegin.ToString(self.TimestampFormatString)) self.FontBrush
            layout.TimeBegin_Pos <- new Point(MARGIN_LEFT, height-self.FontSize-MARGIN_BOTTOM)
            layout.TimeEnd_Text <- _format_text (self.TimeEnd.ToString(self.TimestampFormatString)) self.FontBrush
            layout.TimeEnd_Pos <- new Point(width - layout.TimeEnd_Text.Width - MARGIN_RIGHT, height-self.FontSize-MARGIN_BOTTOM)

            let groups_top_y, groups_bottom_y = MARGIN_TOP + self.FontSize + TEXT_TEXT_SPACE, height -  layout.TimeEnd_Text.Height - GROUP_BOTTOM_SPACE
            let total_weight = self.LineGroups.Sum(fun (lg:PlcLineGroup) -> if lg.Height.HasValue then 0.0 else lg.Weight)
            let sum_of_fixed_height = self.LineGroups.Sum (fun (lg:PlcLineGroup) -> if lg.Height.HasValue then lg.Height.Value else 0.0)
            let pixels_per_weight = (groups_bottom_y - groups_top_y - (float (self.LineGroups.Count-1))*GROUP_GROUP_SPACE - sum_of_fixed_height) / total_weight

            // Right: Legends
            let group_bottom_y = new List<double>()
            let mutable bottom_y = groups_bottom_y
            for i=0 to self.LineGroups.Count-1 do
                let lg = self.LineGroups.[i]
                group_bottom_y.Add(bottom_y)
                bottom_y <- bottom_y - lg.ActualHeight(pixels_per_weight) - GROUP_GROUP_SPACE
                for line in lg.Lines do
                    layout.Legends_Text.Add(_format_text line.Name line.Pen.Brush)
            let max_legend_width = layout.Legends_Text.Max(fun (tf:FormattedText) -> tf.Width)
            let max_legend_height = layout.Legends_Text.Max(fun (tf:FormattedText) -> tf.Height)
            let mutable legend_index = 0
            let legend_left = width - MARGIN_RIGHT - max_legend_width
            for lg_index=0 to self.LineGroups.Count-1 do
                let lg = self.LineGroups.[lg_index]
                let mutable height_so_far = 0.0
                for line_index=0 to lg.Lines.Count-1 do
                    let line = lg.Lines.[line_index]
                    let ft = layout.Legends_Text.[legend_index]
                    layout.Legends_Pos.Add(new Point(legend_left, group_bottom_y.[lg_index] -  height_so_far - max_legend_height))
                    height_so_far <- height_so_far + ft.Height + TEXT_TEXT_SPACE
                    legend_index <- legend_index + 1
            let nlines = self.LineGroups.Sum(fun (lg:PlcLineGroup) -> lg.Lines.Count)

            // Left: Units on the leftmost.
            let mutable max_units_right = MARGIN_LEFT
            for lg_index=0 to self.LineGroups.Count-1 do
                let lg = self.LineGroups.[lg_index]
                let y, h = group_bottom_y.[lg_index], lg.ActualHeight(pixels_per_weight)
                let ft = _format_text lg.Unit self.FontBrush
                let pos = new Point(MARGIN_LEFT, y - 0.5* (h + ft.Height))
                layout.Units.Add( (ft, pos) )
                let right = MARGIN_LEFT + ft.Width
                if right>max_units_right then
                    max_units_right <- right

            // Left: Y Scales, Units on the
            let mutable max_scale_right = 0.0
            for group_index=0 to self.LineGroups.Count-1 do
                let lg = self.LineGroups.[group_index]
                let mutable vmin = 0.0
                let mutable vmax = 1.0
                let mutable vfirst = true
                let mutable is_all_digital = true
                for line in lg.Lines do
                    for y in line.Points do
                        if vfirst then
                            vmin <- y
                            vmax <- y
                            vfirst <- false
                        else 
                            if y<vmin then vmin <- y
                            if y>vmax then vmax <- y
                        if y<>PlcChart.LOGICAL_ZERO && y<>PlcChart.LOGICAL_ONE then
                            is_all_digital <- false
                lg.IsDigital <- is_all_digital
                let height = lg.ActualHeight(pixels_per_weight)
                let nsteps, step_size, min_value, max_value = if is_all_digital then (1, 1.0, 0.0, 1.0) else _autodetect_y_step vmin vmax (height / self.PixelsPerDivision)
                let scales = new List<FormattedText * Point>()
                let step_height = height / (float nsteps)
                for step_index=0 to nsteps do
                    let s = (min_value + (float step_index) * step_size).ToString("g")
                    let ft = _format_text s Brushes.Black
                    let point = new Point(max_units_right, group_bottom_y.[group_index] - (float step_index)*step_height - (if step_index=0 then ft.Height else 0.0) )
                    let right = point.X + ft.Width
                    if right>max_scale_right then
                        max_scale_right <- right
                    scales.Add(ft, point)
                layout.GroupYScales.Add( (nsteps, step_size, min_value, max_value, scales))

            // Boxes.
            let boxes_right = legend_left - GROUP_LEGEND_SPACE
            let boxes_left = max_scale_right
            for lg_index=0 to self.LineGroups.Count-1 do
                let lg = self.LineGroups.[lg_index]
                let y, h = group_bottom_y.[lg_index], lg.ActualHeight(pixels_per_weight)
                layout.GroupBoxes.Add(new Rect(boxes_left, y - h, boxes_right-boxes_left, h))
            // Horizontal scale / divisions
            do
                let xvalues_min, xvalues_max = self.TimeBegin.Ticks, self.TimeEnd.Ticks
                layout.XScale <- _autodetect_time_step xvalues_min xvalues_max (Math.Ceiling((boxes_right - boxes_left) / self.PixelsPerDivision))
                let vg_n, vg_step, vg_begin, vg_end = layout.XScale
                let ft = _format_text (self.TimeDivisionText + _name_of_timespan (int ((TimeSpan.FromTicks(vg_step)).TotalSeconds))) self.FontBrush
                let pos = new Point(0.5 * (width - ft.Width), height-ft.Height)
                layout.HorizontalDivision.Add( (ft, pos) )
            layout
        // Draw a line into DC.
        let _draw_line (dc:DrawingContext) (points:seq<Point>) (pen:Pen) =
            let g = new StreamGeometry()
            let gc = g.Open()
            let mutable is_first = true
            for p in points do
                if is_first then
                    gc.BeginFigure(p, false, false)
                    is_first <- false
                else
                    gc.LineTo(p, true, false)
            gc.Close()
            g.Freeze()
            dc.DrawGeometry(Brushes.Blue, pen, g)
        // Draw a dotted line into DC.
        let _draw_dotted_line (dc:DrawingContext) (points:seq<Point option>) (pen:Pen) =
            let g = new StreamGeometry()
            let gc = g.Open()
            let mutable is_first = true
            for p in points do
                match p with
                | Some pt ->
                    if is_first then
                        gc.BeginFigure(pt, false, false)
                        is_first <- false
                    else
                        gc.LineTo(pt, true, false)
                | None ->
                    is_first <- true
            gc.Close()
            g.Freeze()
            dc.DrawGeometry(Brushes.Blue, pen, g)
        let _clear_overlay () =
            let dc = self._FetchOrCreateVisual(VISUAL_INDEX_OVERLAY).RenderOpen()
            dc.Close()
            self._last_mouse <- None
        let rec index_of_tick (ticks:List<int64 * bool>) (tick:int64) (min:int) (max:int) =    
            if max < min then
                // have to choose between max and min
                let i0, i1 = max, min
                if i0<0 then
                    i1
                else
                    if i1>=ticks.Count then
                        i0
                    else
                        // both cannot be wrong at the same time
                        let diff0, diff1 = Math.Abs(tick - (fst ticks.[i0])), Math.Abs(tick - (fst ticks.[i1]))
                        if diff0 < diff1 then i0 else i1
            else
                let position = min + ((max-min)/2)
                if (fst ticks.[position]) < tick then
                    index_of_tick ticks tick (position+1) max
                else if (fst ticks.[position]) > tick then
                    index_of_tick ticks tick min (position-1)
                else
                    position
        do
            self.Loaded.Add(fun _ -> self.Redraw())
            self.Unloaded.Add(fun _ -> self.Redraw())
            self.SizeChanged.Add(fun _ -> self.Redraw())
            self.MouseMove.Add(self.HandleMouseMove)
            self.MouseEnter.Add(self.HandleMouseEnter)
            self.MouseLeave.Add(self.HandleMouseLeave)
        member val private _layout = new PlcLayout() with get, set
        member val private _last_mouse:Point option = None with get, set
        member val private _visuals:VisualCollection = (null :> VisualCollection) with get, set // either use VisualCollection or manually do addvisualchild/addlogicalchild, etc.
        override this.VisualChildrenCount with get() = if this._visuals=null then 0 else this._visuals.Count
        override this.GetVisualChild (index:int) = this._visuals.[index]
        member private this._FetchOrCreateVisual (index:int) : DrawingVisual =
            if this._visuals = null then
                this._visuals <- new VisualCollection(this)
            while this._visuals.Count<=index do
                this._visuals.Add(new DrawingVisual()) |> ignore
            this._visuals.[index] :?> DrawingVisual

        member val LineGroups:List<PlcLineGroup> = new List<PlcLineGroup>() with get, set
        member val XValues:List<int64 * bool> = new List<int64 * bool>() with get, set

        static member val LOGICAL_ONE = 0.97 with get
        static member val LOGICAL_ZERO = 0.03 with get

        member val internal TimestampFormatString:string = "yyyy'-'MM'-'dd' 'HH':'mm':'ss"

        member val public TimeBegin:DateTime = DateTime.Now.Subtract(TimeSpan.FromSeconds(180.0)) with get, set
        member val public TimeEnd:DateTime = DateTime.Now with get, set

        static member val TypefaceProperty = DependencyProperty.Register("Typeface", typeof<Typeface>, typeof<PlcChart>, new PropertyMetadata(new Typeface(new FontFamily("Arial"), FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)))
        member public this.Typeface with get() = this.GetValue(PlcChart.TypefaceProperty) :?> Typeface and set(v:Typeface) = this.SetValue(PlcChart.TypefaceProperty, v)

        static member val FontSizeProperty = DependencyProperty.Register("FontSize", typeof<double>, typeof<PlcChart>, new PropertyMetadata(12.0))
        member public this.FontSize with get() = this.GetValue(PlcChart.FontSizeProperty) :?> double and set(v:double) = this.SetValue(PlcChart.FontSizeProperty, v)

        static member val FontBrushProperty = DependencyProperty.Register("FontBrush", typeof<Brush>, typeof<PlcChart>, new PropertyMetadata(Brushes.Black))
        member public this.FontBrush with get() = this.GetValue(PlcChart.FontBrushProperty) :?> Brush and set(v:Brush) = this.SetValue(PlcChart.FontBrushProperty, v)

        static member val PixelsPerDivisionProperty = DependencyProperty.Register("PixelsPerDivision", typeof<float>, typeof<PlcChart>, new PropertyMetadata(50.0))
        member public this.PixelsPerDivision with get() = this.GetValue(PlcChart.PixelsPerDivisionProperty) :?> float and set(v:float) = this.SetValue(PlcChart.PixelsPerDivisionProperty, v)

        static member val TimeDivisionTextProperty = DependencyProperty.Register("TimeDivisionText", typeof<string>, typeof<PlcChart>, new PropertyMetadata("Horizontal division:"))
        member public this.TimeDivisionText with get() = this.GetValue(PlcChart.TimeDivisionTextProperty) :?> string and set(v:string) = this.SetValue(PlcChart.TimeDivisionTextProperty, v)

        member private this._RedrawOverlay(mouse_position: Point option) =
            let dc = this._FetchOrCreateVisual(VISUAL_INDEX_OVERLAY).RenderOpen()
            match mouse_position with
            | Some pos ->
                // compound_rc
                let mutable compound_rc = this._layout.GroupBoxes.[0]
                for rc in this._layout.GroupBoxes do
                    compound_rc.Union(rc)

                if pos.X>=compound_rc.Left && pos.X<compound_rc.Right then
                    let xpen = new Pen(Brushes.Blue, 0.5)
                    // tick
                    let tick_min, tick_max = this.TimeBegin.Ticks, this.TimeEnd.Ticks
                    let ftick = ((pos.X - compound_rc.Left) / compound_rc.Width) * (float (tick_max - tick_min))
                    let tick = (int64 ftick) + tick_min
                    let best_index = index_of_tick this.XValues tick 0 (this.XValues.Count-1)
                    let tick_present = best_index<this.XValues.Count && (snd (this.XValues.[best_index]))
                    if tick_present then
                        let is_first_half = 2L * tick < (tick_min + tick_max)
                        for lg_index=0 to this.LineGroups.Count-1 do
                            let lg = this.LineGroups.[lg_index]
                            let rc = this._layout.GroupBoxes.[lg_index]
                            // Vertical cross-hair
                            _draw_line dc [| new Point(pos.X, rc.Top); new Point(pos.X, rc.Bottom) |] xpen
                            // Values
                            let _, _, vmin, vmax, _ = this._layout.GroupYScales.[lg_index]
                            for ln_index=0 to lg.Lines.Count-1 do
                                let ln = lg.Lines.[ln_index]
                                let point = ln.Points.Skip(best_index).First()
                                let spoint = if lg.IsDigital then (if point=PlcChart.LOGICAL_ONE then "1" else "0") else (point.ToString(lg.FormatString) + (if lg.Unit.Length>0 then "" else "") + lg.Unit)
                                // Crosshair text
                                let ft = _format_text spoint ln.Pen.Brush
                                let ft_y = rc.Bottom - (ft.Height + TEXT_TEXT_SPACE) * (float (ln_index + 1)) - TEXT_TEXT_SPACE
                                let ft_x = if is_first_half then pos.X + TEXT_TEXT_SPACE else pos.X - TEXT_TEXT_SPACE - ft.Width
                                dc.DrawText(ft, new Point(ft_x, ft_y))
                                // Crosshair cross
                                let pt_y = rc.Bottom - (point - vmin) / (vmax - vmin) * rc.Height
                                _draw_line dc [| new Point(pos.X - 0.5*CROSSHAIR_WIDTH, pt_y); new Point(pos.X + 0.5*CROSSHAIR_WIDTH, pt_y) |] (new Pen(ln.Pen.Brush, 2.0*ln.Pen.Thickness))
                        let ft = _format_text ((new DateTime(tick)).ToString(this.TimestampFormatString)) this.FontBrush
                        let ft_x =
                            if is_first_half then
                                Math.Max(compound_rc.Left, (pos.X - 0.5*ft.Width))
                            else
                                Math.Min(compound_rc.Right - ft.Width, (pos.X - 0.5*ft.Width))
                        dc.DrawText(ft, new Point(ft_x, MARGIN_TOP))
                    else
                        this._last_mouse <- None
                else
                    this._last_mouse <- None
            | None ->()
            dc.Close()

        // Let's redraw the overlay!
        member private this.HandleMouseMove(args:MouseEventArgs) =
            if this.ActualWidth>0.0 && this.ActualHeight>0.0 && this.LineGroups.Count>0 then
                let pos = args.GetPosition(this)
                this._last_mouse <- Some pos
                this._RedrawOverlay(Some pos)
            else
                _clear_overlay ()
        member private this.HandleMouseEnter(args: MouseEventArgs) =
            ()
        member private this.HandleMouseLeave(args: MouseEventArgs) =
            _clear_overlay ()
        member public this.Redraw() =
            // magic goes in here
            if this.ActualWidth>0.0 && this.ActualHeight>0.0 && this.LineGroups.Count>0 then
                let some_layout =
                    try
                        this._layout <- _create_layout ()
                        Some (this._layout)
                    with
                    | ex -> None // don't complain :)
                match some_layout with
                | Some layout ->
                    let dc = this._FetchOrCreateVisual(VISUAL_INDEX_CHART).RenderOpen()

                    // Draw texts
                    dc.DrawText(layout.TimeBegin_Text, layout.TimeBegin_Pos)
                    dc.DrawText(layout.TimeEnd_Text, layout.TimeEnd_Pos)

                    // _draw_line dc [| new Point(20.0, 0.0); new Point(20.0, 100.0) |] (new Pen(Brushes.Blue, 3.0))

                    for i=0 to layout.Legends_Text.Count-1 do
                        dc.DrawText(layout.Legends_Text.[i], layout.Legends_Pos.[i])

                    // Groups: Boxes
                    let mutable compound_rc = layout.GroupBoxes.[0]
                    for rc in layout.GroupBoxes do
                        dc.DrawRectangle(Brushes.Transparent, new Pen(Brushes.Black, GROUPBOX_THICKNESS), rc)
                        compound_rc.Union(rc)

                    // Groups: Legends on the left.
                    for (ft, pos) in layout.Units do
                        dc.DrawText(ft, pos)

                    // Groups: Scales and Lines
                    let xvalues_min, xvalues_max = this.TimeBegin.Ticks, this.TimeEnd.Ticks
                    let grid_pen = new Pen(Brushes.Gray, GRID_THICKNESS)
                    for lg_index=0 to this.LineGroups.Count-1 do
                        let lg = this.LineGroups.[lg_index]
                        let nsteps, step, vmin, vmax, scales = layout.GroupYScales.[lg_index]
                        let rc = layout.GroupBoxes.[lg_index]
                        // Y-lines
                        for line in lg.Lines do
                            let new_points = seq {
                                let pen = line.Points.GetEnumerator()
                                pen.MoveNext() |> ignore
                                for x_index=0 to this.XValues.Count-1 do
                                    let x_tick, x_tick_ok = this.XValues.[x_index]
                                    if x_tick_ok then
                                        let y = pen.Current
                                        let px = (float (x_tick - xvalues_min)) / (float (xvalues_max - xvalues_min)) * rc.Width + rc.Left
                                        let py = rc.Bottom - (y - vmin) / (vmax-vmin) * rc.Height
                                        pen.MoveNext() |> ignore
                                        yield Some (new Point(px, py))
                                    else
                                        pen.MoveNext() |> ignore
                                        yield None
                            }
                            _draw_dotted_line dc new_points line.Pen
                        // Scale texts
                        for (ft,pos) in scales do
                            dc.DrawText(ft, pos)
                        // Horizontal grid lines
                        let step_height = rc.Height / (double nsteps)
                        for i=1 to nsteps-1 do
                            let y = rc.Bottom - (float i)*step_height
                            _draw_line dc [| new Point(rc.Left, y); new Point(rc.Right, y) |] grid_pen
                        ()
                    // Vertical grid lines
                    let vg_n, vg_step, vg_begin, vg_end = layout.XScale
                    for i=0 to vg_n do
                        let ticks = vg_begin + (int64 i) * vg_step
                        let x = (float (ticks - xvalues_min)) / (float (xvalues_max - xvalues_min)) * (compound_rc.Width) + compound_rc.Left
                        _draw_line dc [| new Point(x, compound_rc.Bottom); new Point(x, compound_rc.Top) |] grid_pen
                        ()
                    // Horizontal division text
                    for (text,pos) in layout.HorizontalDivision do
                        dc.DrawText(text, pos)
                    // COMPLETE!
                    dc.Close()
                    this._RedrawOverlay(self._last_mouse)
                | None -> ()
            
