# ============================================================
#  R/mod_gallery.R
#  Module    : Plot Classification Gallery
#  Description : A comprehensive, searchable reference catalog
#                of 505+ named chart/graph types organized into
#                20 major visualization categories.  Helps users
#                discover the right plot for their data and find
#                which tab implements it.
#
#  Data type   : N/A (static content – no server data needed)
# ============================================================


# ── Gallery data  ────────────────────────────────────────────
# Each row: Category | Name | Description | Use Case | Data Type | Implemented | Available In

.make_gallery_data <- function() {
  m <- rbind(
    # ── 1. Bar & Column Charts (35) ─────────────────────────
    c("Bar & Column","Vertical Bar Chart","Simple bars arranged vertically","Compare values across categories","Cat + Num","yes","General Plots"),
    c("Bar & Column","Horizontal Bar Chart","Bars arranged horizontally; good for long labels","Ranked comparisons with text labels","Cat + Num","yes","General Plots"),
    c("Bar & Column","Grouped / Clustered Bar","Bars side-by-side per group","Compare sub-groups within categories","Cat + Num + Group","yes","General Plots"),
    c("Bar & Column","Stacked Bar Chart","Segments stacked on each bar","Part-of-whole across categories","Cat + Num + Group","yes","General Plots"),
    c("Bar & Column","100% Stacked Bar","Bars scaled to 100%","Proportional composition comparison","Cat + Num + Group","yes","General Plots"),
    c("Bar & Column","Diverging Bar Chart","Bars extend left (negative) and right (positive)","Show deviation from zero / baseline","Cat + Num","yes","General Plots"),
    c("Bar & Column","Bar with Error Bars","Mean bars + SD/SE whiskers","Statistical comparison of group means","Cat + Num","yes","General Plots"),
    c("Bar & Column","Lollipop Chart","Dot on a stem; lighter than bar","Ranked values with clear endpoints","Cat + Num","yes","General Plots / Best Plots"),
    c("Bar & Column","Dumbbell / Barbell Chart","Two dots connected per category","Before-after or two-group difference","Cat + 2 Num","yes","General Plots / Best Plots"),
    c("Bar & Column","Cleveland Dot Plot","Dot aligned to axis gridlines","Precise ranked comparison","Cat + Num","yes","General Plots"),
    c("Bar & Column","Waterfall Chart","Incremental bars showing running total","Contribution of sequential changes","Cat + Num","yes","General Plots"),
    c("Bar & Column","Polar / Radial Bar","Bars on circular axis","Cyclical or angular comparisons","Cat + Num","yes","General Plots"),
    c("Bar & Column","Population Pyramid","Mirrored horizontal bars by age/sex","Age-sex population distribution","Cat + Num + 2 Groups","yes","General Plots"),
    c("Bar & Column","Faceted Bar Chart","Bar panels per group","Compare categories across many groups","Cat + Num + Group","yes","General Plots"),
    c("Bar & Column","Pareto Chart","Sorted bars + cumulative % line","80/20 analysis; defect prioritization","Cat + Num","yes","General Plots"),
    c("Bar & Column","Bullet Chart","Progress bar vs target reference line","KPI progress vs goal","Cat + Num + Target","no","—"),
    c("Bar & Column","Dot Matrix Chart","Grid of icons/dots","Frequency or proportion display","Cat + Num","no","—"),
    c("Bar & Column","Spiral Bar Chart","Bars arranged on a spiral","Cyclical temporal comparisons","Time + Num","no","—"),
    c("Bar & Column","Tornado Chart","Dual horizontal bars; sensitivity","Sensitivity / scenario analysis","Cat + 2 Num","no","—"),
    c("Bar & Column","Signed Bar Chart","Positive bars right, negative bars left","Show +/- direction clearly","Cat + Num","no","—"),
    c("Bar & Column","Gradient Bar Chart","Bar filled with a color gradient","Emphasize magnitude direction","Cat + Num","no","—"),
    c("Bar & Column","Mosaic Bar (Variable Width)","Width ∝ one variable, height ∝ another","Two-way composition","Cat × Cat","no","—"),
    c("Bar & Column","Mirror / Butterfly Chart","Two bars per row going opposite directions","Side-by-side group comparison","Cat + Num + Group","no","—"),
    c("Bar & Column","Step Bar Chart","Bar tops drawn as staircase","Discrete level transitions","Cat + Num","no","—"),
    c("Bar & Column","Radial Column Chart","Circular stacked bars","Cyclical multi-category composition","Cat + Num + Group","no","—"),
    c("Bar & Column","Spine Chart","Bars of fixed height, variable segment widths","Proportional comparison","Cat + Prop","no","—"),
    c("Bar & Column","Percent-Labeled Bar","Bars annotated with exact %","Clarity in proportional bars","Cat + Num","no","—"),
    c("Bar & Column","Comparison / Before-After Bar","Two bars per entity","Intervention effect","Cat + 2 Num","no","—"),
    c("Bar & Column","Timeline Bar","Bars mapped to time axis","Duration or scheduling","Cat + Time + Num","no","—"),
    c("Bar & Column","Annotated Bar Chart","Text annotations on bars","Story-telling with data labels","Cat + Num","no","—"),
    c("Bar & Column","Target Reference Bar","Bar with reference line / goal marker","Performance vs target","Cat + Num + Target","no","—"),
    c("Bar & Column","Multi-Group Comparison Bar","Many sub-groups per axis tick","Complex grouped comparisons","Cat + Num + MultiGroup","no","—"),
    c("Bar & Column","Slope Bar","Bar heights encoding slope/change","Rate-of-change comparison","Cat + Num","no","—"),
    c("Bar & Column","Icon / Pictogram Bar","Bars made of repeated icons","Accessible / infographic display","Cat + Num","no","—"),
    c("Bar & Column","Marimekko / Mekko Chart","Variable-width stacked bars","Two-variable proportional composition","Cat × Cat","no","—"),

    # ── 2. Histogram & Density (30) ─────────────────────────
    c("Histogram & Density","Standard Histogram","Counts per bin of continuous variable","Distribution shape of numeric data","Num","yes","General Plots / Score Explorer"),
    c("Histogram & Density","Normalized Histogram","Y-axis as frequency/density","Compare distributions of different sizes","Num","yes","General Plots"),
    c("Histogram & Density","Cumulative Histogram","Running-total bars","Understand percentile thresholds","Num","yes","General Plots"),
    c("Histogram & Density","2D Histogram (bin2d)","Color-coded square bins","Density of bivariate data","Num × Num","yes","General Plots"),
    c("Histogram & Density","Mirror Histogram","Two histograms reflected on x-axis","Compare two distributions","Num + Group (2)","yes","General Plots"),
    c("Histogram & Density","Dodged Histogram","Grouped bins side-by-side","Compare distributions side by side","Num + Group","no","—"),
    c("Histogram & Density","Stacked Histogram","Groups stacked in bins","Composition within distribution","Num + Group","no","—"),
    c("Histogram & Density","Filled Histogram","Groups as fill proportion per bin","Relative composition in bins","Num + Group","yes","General Plots"),
    c("Histogram & Density","Stepped Histogram","Outline-style without bar fill","Clean distribution outline","Num","no","—"),
    c("Histogram & Density","Ridge / Joy Plot","Overlapping density rows per group","Compare many distributions compactly","Num + Group","yes","General Plots"),
    c("Histogram & Density","Density Curve","Smooth kernel density estimate","Continuous distribution shape","Num","yes","General Plots / Score Explorer"),
    c("Histogram & Density","Filled Density Plot","Shaded area under density curve","Emphasize distribution area","Num + Group","yes","General Plots"),
    c("Histogram & Density","Stacked Density","Cumulative stacked density curves","Multi-group composition","Num + Group","no","—"),
    c("Histogram & Density","Density Ridge Plot","Stacked density rows (ggridges)","Many-group distribution comparison","Num + Group","yes","General Plots"),
    c("Histogram & Density","Violin Plot","Mirrored density per category","Distribution shape per group","Cat + Num","yes","General Plots / Best Plots"),
    c("Histogram & Density","Half Violin Plot","One-sided density per category","Space-efficient distribution view","Cat + Num","yes","General Plots"),
    c("Histogram & Density","Split Violin","Two groups mirrored on same violin","Direct distribution comparison","Cat + Num + Group (2)","no","—"),
    c("Histogram & Density","Beeswarm Plot","Jittered individual points shaped by density","Show all points without overlap","Cat + Num","no","—"),
    c("Histogram & Density","Strip Chart","Points jittered along categorical axis","All raw values per group","Cat + Num","yes","General Plots"),
    c("Histogram & Density","Sina Plot","Density-shaped jitter strip","Points + distribution combined","Cat + Num","no","—"),
    c("Histogram & Density","Raincloud Plot","Half violin + box + dots combined","Full distribution picture","Cat + Num","no","—"),
    c("Histogram & Density","Dot Histogram","Stacked dots instead of bars","Small datasets, readable distribution","Num","no","—"),
    c("Histogram & Density","ECDF Plot","Empirical cumulative distribution curve","Percentile and threshold queries","Num","yes","General Plots / Score Explorer"),
    c("Histogram & Density","ECDF by Group","Multiple ECDF lines per group","Compare cumulative distributions","Num + Group","yes","General Plots / Score Explorer"),
    c("Histogram & Density","Q-Q Plot","Quantile vs theoretical quantile","Normality and distribution diagnostics","Num","yes","General Plots"),
    c("Histogram & Density","P-P Plot","Probability vs probability","Distribution fit assessment","Num","no","—"),
    c("Histogram & Density","Density Heatmap (2D)","2D density as colored grid","Bivariate density, large datasets","Num × Num","yes","General Plots"),
    c("Histogram & Density","Frequency Polygon","Histogram shown as connected line","Overlapping distribution comparison","Num + Group","no","—"),
    c("Histogram & Density","Area Under Curve","Shaded region under density","Highlight probability mass","Num","no","—"),
    c("Histogram & Density","Kernel Density Estimate Comparison","Multiple KDE curves","Compare distribution shapes","Num + Group","no","—"),

    # ── 3. Box & Whisker Plots (20) ─────────────────────────
    c("Box & Whisker","Standard Box Plot","Q1/median/Q3 + whiskers + outliers","Distribution summary per group","Cat + Num","yes","General Plots / Score Explorer"),
    c("Box & Whisker","Notched Box Plot","Box with confidence notch on median","Test median equality visually","Cat + Num","yes","General Plots"),
    c("Box & Whisker","Box + Jitter","Box plot with raw points overlaid","Distribution + individual values","Cat + Num","yes","General Plots"),
    c("Box & Whisker","Box + Beeswarm","Box with beeswarm-style points","Distribution + non-overlapping points","Cat + Num","no","—"),
    c("Box & Whisker","Violin + Box Combo","Violin wrapping a thin box plot","Full shape + summary stats","Cat + Num","yes","General Plots / Best Plots"),
    c("Box & Whisker","Letter-Value Plot","Quantile boxes beyond IQR","Large datasets; extreme quantiles","Cat + Num","yes","General Plots"),
    c("Box & Whisker","Tukey Box Plot","Standard Tukey 1.5×IQR definition","Outlier detection by convention","Cat + Num","no","—"),
    c("Box & Whisker","Horizontal Box Plot","Box plots rotated 90°","Long category names","Cat + Num","no","—"),
    c("Box & Whisker","Colored Box Plot","Fill color encodes a grouping variable","Multi-factor distribution","Cat + Num + Group","no","—"),
    c("Box & Whisker","Box Plot Matrix","Many variables in one panel","Multivariate distribution overview","Multiple Num","no","—"),
    c("Box & Whisker","Faceted Box Plot","Box plots in separate panels","Compare distributions across conditions","Cat + Num + Facet","no","—"),
    c("Box & Whisker","Outlier-Highlighted Box","Outlier points colored distinctly","Identify and communicate outliers","Cat + Num","no","—"),
    c("Box & Whisker","Mean-Marked Box","Diamond or dot marks the mean","Distinguish mean from median","Cat + Num","no","—"),
    c("Box & Whisker","Quartile Box Plot","All four quartiles emphasized","Precise percentile communication","Cat + Num","no","—"),
    c("Box & Whisker","Comparison Box Plot","Two-group contrast in same panel","A/B or control/treatment","Cat + Num + Group (2)","no","—"),
    c("Box & Whisker","Box Crossbar","Median and IQR as crossbar (no whisker)","Simplified box style","Cat + Num","no","—"),
    c("Box & Whisker","Mini Box Plot","Compact boxes in small panels","Sparkline-style distribution","Cat + Num","no","—"),
    c("Box & Whisker","Half Violin + Box","Violin on one side, box on other","Compact summary + shape","Cat + Num","yes","General Plots"),
    c("Box & Whisker","Box Plot with Strip","Box + raw jitter points","Full data + summary","Cat + Num","yes","General Plots"),
    c("Box & Whisker","Box Plot by Facet + Color","Panels × color encoding","Complex multi-factor view","Cat + Num + Facet + Group","no","—"),

    # ── 4. Scatter & Bubble Plots (30) ──────────────────────
    c("Scatter & Bubble","Standard Scatter Plot","Points at (x, y) coordinates","Relationship between two numeric variables","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Bubble Chart","Scatter with point size ∝ a third variable","Three-variable relationship","Num × Num + Size","yes","General Plots / Best Plots"),
    c("Scatter & Bubble","Color-Coded Scatter","Points colored by group","Cluster or group separation in 2D","Num × Num + Group","yes","General Plots"),
    c("Scatter & Bubble","Scatter + Linear Fit","Points + lm regression line","Show linear trend with CI","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Scatter + LOESS Smoother","Points + non-linear smooth curve","Flexible trend without model assumption","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Scatter + Ellipses","Points + 95% CI ellipses per group","Group overlap visualization","Num × Num + Group","yes","General Plots"),
    c("Scatter & Bubble","Scatter + Rug","Marginal tick marks on axes","Show marginal distributions","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Scatter + Marginal Histograms","Side histograms on x and y axes","Joint + marginal distributions","Num × Num","no","—"),
    c("Scatter & Bubble","Hexagonal Binning","Hex cells colored by count","Dense scatter (overplotting)","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","2D Density Overlay","Contour lines on scatter","Identify high-density regions","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Contour / Filled Contour","Filled color contour regions","Bivariate density landscape","Num × Num","yes","General Plots"),
    c("Scatter & Bubble","Labeled Scatter","Points with text labels (ggrepel)","Identify individual points by name","Num × Num + Label","yes","General Plots"),
    c("Scatter & Bubble","Connected Scatter","Points connected by path in order","Time-ordered or sequential relationship","Num × Num + Order","yes","General Plots"),
    c("Scatter & Bubble","Quadrant Scatter","Scatter with reference lines at median/mean","Classify points into 4 quadrants","Num × Num","no","—"),
    c("Scatter & Bubble","Jitter Plot","Points randomly displaced to reduce overlap","Categorical x with numeric y","Cat + Num","yes","General Plots"),
    c("Scatter & Bubble","Alpha-Mapped Scatter","Transparency encodes a variable","Dense data with intensity information","Num × Num + Num","no","—"),
    c("Scatter & Bubble","Shape-Mapped Scatter","Point shape encodes group","Group separation without color","Num × Num + Group","no","—"),
    c("Scatter & Bubble","Arrow Scatter","Directional arrows between point pairs","Before-after or vector fields","Num × Num + Direction","no","—"),
    c("Scatter & Bubble","Faceted Scatter","Multi-panel scatter","Compare scatter across conditions","Num × Num + Facet","yes","General Plots"),
    c("Scatter & Bubble","Scatter Matrix / SPLOM","All pairwise scatter plots","Multivariate exploratory analysis","Multiple Num","yes","General Plots"),
    c("Scatter & Bubble","Log-Scale Scatter","Logarithmic axis transformations","Data spanning orders of magnitude","Num × Num (log)","no","—"),
    c("Scatter & Bubble","Polar Scatter","Points in polar coordinates","Circular/angular data","Angle + Radius","no","—"),
    c("Scatter & Bubble","Size-Color Scatter","Both size and color encode variables","Four-variable relationship","Num × Num + Size + Color","yes","General Plots"),
    c("Scatter & Bubble","Dual-Axis Scatter","Two y-axes for different scales","Two metrics with different ranges","Num × Num × Num","no","—"),
    c("Scatter & Bubble","Segment Scatter","Line segments between two points","Connecting related measurements","2 Num pairs","no","—"),
    c("Scatter & Bubble","Time Scatter","Scatter with time on x-axis","Temporal relationships","Time + Num","no","—"),
    c("Scatter & Bubble","Score-Gradient Scatter","Continuous color gradient on points","Smooth metric across 2D space","Num × Num + Num","no","—"),
    c("Scatter & Bubble","Biplot","PCA biplot with variables + samples","Dimensionality reduction interpretation","Multi Num","no","—"),
    c("Scatter & Bubble","Correlation Scatter","Scatter with correlation coefficient","Quantify linear relationship","Num × Num","no","—"),
    c("Scatter & Bubble","Network Scatter","Graph nodes as scatter points","Network positions in 2D layout","Graph data","no","—"),

    # ── 5. Line & Area Charts (30) ──────────────────────────
    c("Line & Area","Standard Line Chart","Values connected by lines over x-axis","Trends, time series","Num × Num","yes","General Plots"),
    c("Line & Area","Multi-Line Chart","Multiple series as separate lines","Compare trends across groups","Num × Num + Group","yes","General Plots"),
    c("Line & Area","Line + Points","Line with point markers","Emphasize individual data points","Num × Num","yes","General Plots"),
    c("Line & Area","Smoothed Line (GAM/LOESS)","Non-parametric smooth curve","Underlying trend in noisy data","Num × Num","yes","General Plots"),
    c("Line & Area","Step Line Chart","Lines drawn as horizontal steps","Discrete state changes","Num × Num","yes","General Plots"),
    c("Line & Area","Bump Chart","Lines ranking groups over time","Rank-over-time comparison","Cat + Rank + Time","yes","General Plots"),
    c("Line & Area","Slope Chart","Two-period before/after lines","Change between two time points","Cat + 2 Num","yes","General Plots"),
    c("Line & Area","Ribbon / Band Chart","Line with upper/lower confidence band","Uncertainty around a trend","Num + CI","yes","General Plots"),
    c("Line & Area","Area Chart","Line chart with filled area","Magnitude of trend","Num × Num","yes","General Plots"),
    c("Line & Area","Stacked Area Chart","Multiple filled areas stacked","Part-of-whole over time","Num × Num + Group","yes","General Plots"),
    c("Line & Area","100% Stacked Area","Areas scaled to 100%","Proportional composition over time","Num × Num + Group","yes","General Plots"),
    c("Line & Area","Difference Area Chart","Shaded area between two lines","Emphasize gap between series","2 Num × Num","no","—"),
    c("Line & Area","Fan Chart","Multiple percentile lines forming a fan","Forecast uncertainty","Time + Quantiles","no","—"),
    c("Line & Area","Streamgraph","Flowing stacked areas centered on axis","Volume of categories over time","Time + Cat + Num","no","—"),
    c("Line & Area","Horizon Chart","Area chart compressed with color layers","Dense time series comparison","Time + Num","no","—"),
    c("Line & Area","Spiral Chart","Line chart wrapped in a spiral","Cyclical/periodic patterns","Time + Num","no","—"),
    c("Line & Area","Cycle Plot","Repeating seasonal cycles per panel","Seasonal decomposition","Time (seasonal) + Num","no","—"),
    c("Line & Area","Faceted Line Chart","Line panels per group","Compare trends in sub-groups","Num × Num + Facet","yes","General Plots"),
    c("Line & Area","Trend Line","Linear or polynomial trend overlay","Long-term direction","Num × Num","yes","General Plots"),
    c("Line & Area","Forecast Line","Line + prediction interval","Future projections","Time + Num + PI","no","—"),
    c("Line & Area","Connected Scatter-Line","Scatter with path connecting points","Time-ordered 2D movement","Num × Num + Order","yes","General Plots"),
    c("Line & Area","Timeline","Events plotted on a horizontal axis","Event sequence or history","Time + Event","no","—"),
    c("Line & Area","Dot-Line Chart","Dots on a line (categorical x)","Discrete categorical trend","Cat + Num","no","—"),
    c("Line & Area","Parallel Coordinates Line","Each row as a polyline across axes","Multivariate pattern detection","Multi Num","yes","General Plots"),
    c("Line & Area","Dual Y-Axis Line","Two lines with independent y-scales","Compare metrics with different ranges","Num × Num × Num","no","—"),
    c("Line & Area","Annotated Line","Line with event annotations","Contextualize trend with events","Time + Num + Events","no","—"),
    c("Line & Area","Step Area","Step line with filled area","Discrete state + magnitude","Num × Num","no","—"),
    c("Line & Area","Cumulative Line","Running total over x-axis","Cumulative growth / accumulation","Num × Num","no","—"),
    c("Line & Area","Profile Chart","Multiple variables as line profiles per case","Compare entity profiles","Multi Num per entity","no","—"),
    c("Line & Area","Ranking Line (Ranking over X)","Lines tracking rank changes","Competitive ranking over time","Cat + Rank + Time","no","—"),

    # ── 6. Heatmaps & Tiles (25) ────────────────────────────
    c("Heatmap & Tile","Standard Heatmap","Matrix of numeric values, color-encoded","Matrix data, correlation, expression","Cat × Cat + Num","yes","General Plots"),
    c("Heatmap & Tile","Clustered Heatmap","Heatmap with dendrogram-ordered rows/cols","Pattern discovery in expression data","Cat × Cat + Num","yes","General Plots / Best Plots"),
    c("Heatmap & Tile","Correlation Heatmap","Pairwise correlations as colored matrix","Variable relationship overview","Multi Num","yes","General Plots"),
    c("Heatmap & Tile","Calendar Heatmap","Day × week grid colored by value","Daily activity / time patterns","Date + Num","yes","General Plots"),
    c("Heatmap & Tile","Diverging Heatmap","Blue-white-red scale around zero","Positive vs negative deviations","Cat × Cat + Num (±)","yes","General Plots / Enrichment"),
    c("Heatmap & Tile","Polar Heatmap","Heatmap on circular arrangement","Cyclical two-way data","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Annotated Heatmap","Tile values shown as text inside cells","Precision communication","Cat × Cat + Num","yes","General Plots"),
    c("Heatmap & Tile","Triangular Heatmap","Upper or lower triangle only","Symmetric matrices (correlation)","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Tile Plot","Colored rectangles for any two variables","Flexible two-variable encoding","Cat × Cat + Num","yes","General Plots"),
    c("Heatmap & Tile","Bubble Heatmap","Circles in a grid (size + color)","Two-metric matrix (enrichment dot plot)","Cat × Cat + 2 Num","yes","General Plots / Enrichment"),
    c("Heatmap & Tile","Sparse Heatmap","Missing values shown distinctly","Incomplete matrix data","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Z-Score Heatmap","Row or column standardized values","Expression heatmap, scaled","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Row-Clustered Heatmap","Rows ordered by hierarchical clustering","Gene expression pattern","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Column-Clustered Heatmap","Columns ordered by HC","Sample pattern comparison","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Time-Series Heatmap","Time × variable matrix","Seasonal or longitudinal patterns","Time × Cat + Num","no","—"),
    c("Heatmap & Tile","Faceted Tile","Tiled panels per facet variable","Multi-panel matrix comparison","Cat × Cat + Num + Facet","yes","General Plots"),
    c("Heatmap & Tile","Waffle Chart","Square grid of colored cells","Part-of-whole (100 squares = 100%)","Cat + Num","yes","General Plots"),
    c("Heatmap & Tile","Dot Heatmap","Dots instead of filled tiles","Sparse or precise matrix data","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Contour Heatmap","Contour lines on a 2D grid","Continuous spatial data","Num × Num + Num","no","—"),
    c("Heatmap & Tile","2D Density Heatmap","Hex-bin or bin2d density","Large bivariate datasets","Num × Num","yes","General Plots"),
    c("Heatmap & Tile","Proportional Heatmap","Cell size ∝ value","Area-encoded matrix","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Scaled Row Heatmap","Each row scaled 0–1","Normalized profile comparison","Cat × Cat + Num","no","—"),
    c("Heatmap & Tile","Matrix Scatter-Heatmap","Scatterplot + heat background","Joint distribution + raw points","Num × Num","no","—"),
    c("Heatmap & Tile","Genome Heatmap","Genomic coordinates heatmap","ChIP-seq, ATAC-seq density","Genomic + Num","no","—"),
    c("Heatmap & Tile","Enrichment Multi-Metric Tile","Term × metric tile (Count, RF, -log10p)","Enrichment summary","Enrich data","yes","Enrichment"),

    # ── 7. Part-to-Whole (25) ───────────────────────────────
    c("Part-to-Whole","Pie Chart","Classic proportional slices","Simple part-of-whole (≤ 6 groups)","Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Donut Chart","Pie with a central hole","Proportions + central metric","Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Exploded Pie","Separated pie slice","Highlight one category","Cat + Num","no","—"),
    c("Part-to-Whole","Nested Donut","Concentric ring donut","Hierarchical proportions","Hierarchical Cat + Num","no","—"),
    c("Part-to-Whole","Polar Area / Rose Chart","Wedges with variable radius","Cyclical proportional data","Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Nightingale / Coxcomb","Polar area with equal angles","Florence Nightingale style","Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Treemap","Nested rectangles by proportion","Hierarchical part-of-whole","Hierarchical Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Nested Treemap","Two-level hierarchical treemap","Category + sub-category proportions","2-level hierarchy + Num","no","—"),
    c("Part-to-Whole","Sunburst Chart","Radial hierarchy (donut + treemap)","Hierarchical proportions, radial","Hierarchical Cat + Num","no","—"),
    c("Part-to-Whole","Waffle Chart","Grid of colored squares","Accessible part-of-whole","Cat + Num","yes","General Plots"),
    c("Part-to-Whole","Dot Waffle","Dots instead of squares","Friendly / infographic proportions","Cat + Num","no","—"),
    c("Part-to-Whole","Icon Array / Pictogram","Icons arranged as proportion","Public communication of risk/proportion","Cat + Num","no","—"),
    c("Part-to-Whole","Marimekko / Mekko","Variable-width stacked bars","Two-variable composition","Cat × Cat","no","—"),
    c("Part-to-Whole","Mosaic Plot","Tile size ∝ joint frequency","Two-variable categorical association","Cat × Cat","no","—"),
    c("Part-to-Whole","Spine Chart","Variable-height stacked columns","Proportional variation over categories","Cat + Prop","no","—"),
    c("Part-to-Whole","Funnel Chart","Descending bars; pipeline stages","Conversion / process funnel","Stage + Num","yes","General Plots"),
    c("Part-to-Whole","Bar Funnel","Symmetric bar funnel","Pipeline rates","Stage + Num","no","—"),
    c("Part-to-Whole","Population Pyramid","Age × sex bar chart","Demographic structure","Age + Sex + Num","yes","General Plots"),
    c("Part-to-Whole","Chord Diagram","Circular ribbons of flow","Bidirectional flows between groups","Cat × Cat + Num","no","—"),
    c("Part-to-Whole","Sankey Diagram","Flow diagram with proportional ribbons","Flow of quantities through stages","Nodes + Flows","yes","Sankey / Alluvial"),
    c("Part-to-Whole","Alluvial Diagram","Category flow across time/stages","Categorical transitions","Stage × Cat + Num","yes","Sankey / Alluvial"),
    c("Part-to-Whole","Venn Diagram","Overlapping circles for set membership","Set intersections (≤ 4 sets)","Set membership","yes","Venn / UpSet"),
    c("Part-to-Whole","Euler Diagram","Proportional Venn","Accurate set size + overlap","Set membership + sizes","no","—"),
    c("Part-to-Whole","UpSet Plot","Intersection matrix + bar chart","Many-set intersections","Set membership (many sets)","yes","Venn / UpSet"),
    c("Part-to-Whole","Proportional Symbol","Circle area ∝ value","Single-variable proportion","Cat + Num","no","—"),

    # ── 8. Network & Graph Diagrams (25) ────────────────────
    c("Network & Graph","Force-Directed Network","Nodes + edges, spring layout","General network structure","Graph (nodes + edges)","yes","TCM Network / PPI Network"),
    c("Network & Graph","Arc Diagram","Linear nodes, curved arcs","Linear layout network","Graph (nodes + edges)","no","—"),
    c("Network & Graph","Chord Diagram","Circular nodes, ribbon connections","Bidirectional flow matrix","Cat × Cat + Flow","no","—"),
    c("Network & Graph","Hive Plot","Radial axis layout","Structured network comparison","Graph + node attributes","no","—"),
    c("Network & Graph","Circular Network","Nodes on circle, edges inside","Compact network layout","Graph (nodes + edges)","yes","TCM Network"),
    c("Network & Graph","Bipartite Network","Two-layer node layout","Two-mode or bipartite graphs","Bipartite graph","yes","PPI Network"),
    c("Network & Graph","Adjacency Matrix","Matrix of 0/1 or weights","Structured network, no hairball","Graph (nodes + edges)","no","—"),
    c("Network & Graph","Tree Layout","Hierarchical tree","Parent-child relationships","Tree/hierarchy","no","—"),
    c("Network & Graph","Dendrogram","Hierarchical clustering tree","Cluster relationships","Dist matrix or HC","yes","General Plots (clustered heat)"),
    c("Network & Graph","Radial Tree","Tree in circular layout","Large hierarchies","Tree/hierarchy","no","—"),
    c("Network & Graph","Hierarchical Edge Bundling","Bundled edges on radial tree","Reduce visual clutter in dense graphs","Graph + hierarchy","no","—"),
    c("Network & Graph","Bubble Network","Nodes sized by degree or attribute","Node importance in networks","Graph + node metric","no","—"),
    c("Network & Graph","Sankey Network","Flow diagram with proportional paths","Process or conversion flow","Stage flows","yes","Sankey / Alluvial"),
    c("Network & Graph","Alluvial Diagram","Category flow across stages","Category transitions","Stage × Cat","yes","Sankey / Alluvial"),
    c("Network & Graph","Sunburst Tree","Radial hierarchy","Nested categorical hierarchy","Hierarchical data","no","—"),
    c("Network & Graph","Timeline Network","Nodes arranged on time axis","Temporal network events","Graph + time","no","—"),
    c("Network & Graph","Flow Chart","Directed process flow","Algorithm or process visualization","Process nodes","no","—"),
    c("Network & Graph","Star / Hub-Spoke Layout","Central hub with radiating spokes","Hub-dominated networks","Graph","no","—"),
    c("Network & Graph","Grid Network","Nodes at grid positions","Regular lattice networks","Graph","no","—"),
    c("Network & Graph","Community Network","Nodes colored by community","Cluster or module detection","Graph + communities","no","—"),
    c("Network & Graph","Multilayer Network","Multiple edge-type layers","Multi-relational data","Multi-layer graph","no","—"),
    c("Network & Graph","Ego Network","One focal node with neighbors","Individual node neighborhood","Graph + focal node","no","—"),
    c("Network & Graph","Network Heatmap","Adjacency matrix as heatmap","Dense structured network","Graph (nodes + edges)","no","—"),
    c("Network & Graph","Bipartite Arc Diagram","Two rows + arcs between them","Two-mode networks","Bipartite graph","no","—"),
    c("Network & Graph","TCMNP TCM Network","Herb–molecule–target tripartite","TCM pharmacology networks","Network (herb/mol/tgt)","yes","TCM Network"),

    # ── 9. Statistical & Scientific (35) ────────────────────
    c("Statistical & Scientific","Volcano Plot","−log10(p) vs log2(FC) scatter","Differential expression significance","Num (FC + p-value)","no","—"),
    c("Statistical & Scientific","Manhattan Plot","Genomic position vs −log10(p)","GWAS significance landscape","Genomic + p-value","no","—"),
    c("Statistical & Scientific","Q-Q Plot","Theoretical vs sample quantiles","Normality / distributional diagnostics","Num","yes","General Plots"),
    c("Statistical & Scientific","P-P Plot","Theoretical vs empirical probability","Distribution fit","Num","no","—"),
    c("Statistical & Scientific","Kaplan-Meier Survival Curve","Step survival function","Time-to-event analysis","Time + Event indicator","no","—"),
    c("Statistical & Scientific","Survival + Confidence Band","KM curve with 95% CI","Survival uncertainty","Time + Event + CI","no","—"),
    c("Statistical & Scientific","ROC Curve","Sensitivity vs 1-Specificity","Binary classifier performance","Predicted + Actual","no","—"),
    c("Statistical & Scientific","Precision-Recall Curve","Precision vs Recall","Imbalanced classification","Predicted + Actual","no","—"),
    c("Statistical & Scientific","Bland-Altman Plot","Mean vs difference of two methods","Method agreement / Bland-Altman","2 Num (methods)","no","—"),
    c("Statistical & Scientific","Funnel Plot","Effect size vs precision","Meta-analysis publication bias","Effect + SE","no","—"),
    c("Statistical & Scientific","Forest Plot","Effect sizes + CI per study","Meta-analysis summary","Effect + CI","yes","General Plots"),
    c("Statistical & Scientific","Calibration Plot","Predicted probability vs observed rate","Model calibration","Predicted prob + Observed","no","—"),
    c("Statistical & Scientific","PCA Biplot","PC1 vs PC2 with variable arrows","Dimensionality reduction","Multi Num","no","—"),
    c("Statistical & Scientific","PCA Scree Plot","Variance explained per PC","Choose number of PCs","Multi Num","no","—"),
    c("Statistical & Scientific","PCA Loadings Plot","Variable contributions to PCs","Identify influential variables","Multi Num","no","—"),
    c("Statistical & Scientific","t-SNE Plot","Non-linear 2D embedding","High-dimensional cluster exploration","Multi Num","no","—"),
    c("Statistical & Scientific","UMAP Plot","Uniform manifold approximation","Fast dimensionality reduction","Multi Num","no","—"),
    c("Statistical & Scientific","Correlation Matrix Plot","All-pairwise correlations","Variable association overview","Multi Num","yes","General Plots"),
    c("Statistical & Scientific","Residual Plot","Model residuals vs fitted values","Regression diagnostics","Fitted + Residuals","no","—"),
    c("Statistical & Scientific","Cook's Distance Plot","Influential point detection","Leverage and influence","Obs index + Num","no","—"),
    c("Statistical & Scientific","Interaction Plot","Two-factor interaction lines","Factorial experiment analysis","2 Cat + Num","yes","General Plots"),
    c("Statistical & Scientific","Profile Plot","Mean profiles per group","Repeated measures visualization","Cat + Time + Num","no","—"),
    c("Statistical & Scientific","Dose-Response Curve","Concentration vs effect","Pharmacology, toxicology","Conc + Response","no","—"),
    c("Statistical & Scientific","Pharmacokinetics Curve","Drug concentration over time","PK modelling","Time + Conc","no","—"),
    c("Statistical & Scientific","Power Curve","Power vs sample size","Study design planning","N + Power","no","—"),
    c("Statistical & Scientific","Bootstrap Distribution","Resampled statistic distribution","Confidence interval visualization","Bootstrap samples","no","—"),
    c("Statistical & Scientific","Quantile Regression Plot","Multiple quantile lines","Heteroscedastic relationship","Num × Num","no","—"),
    c("Statistical & Scientific","LOESS Diagnostic Plot","Residuals of smooth fit","Smoother evaluation","Num × Num","no","—"),
    c("Statistical & Scientific","Partial Regression Plot","Added-variable / component plot","Multiple regression diagnostics","Multi Num","no","—"),
    c("Statistical & Scientific","Simulation Envelope","Monte Carlo uncertainty band","Simulation-based analysis","Sim output + Num","no","—"),
    c("Statistical & Scientific","Sensitivity Analysis Plot","Output vs parameter range","Model robustness","Param + Output","no","—"),
    c("Statistical & Scientific","Error Magnitude Plot","Absolute/relative errors","Error characterization","Predicted + Actual","no","—"),
    c("Statistical & Scientific","Confidence Interval Chart","CI bars per group","Parameter estimation results","Cat + Estimate + CI","yes","General Plots"),
    c("Statistical & Scientific","Sample Size Curve","Required N vs effect size","Power analysis","ES + N","no","—"),
    c("Statistical & Scientific","Half-Life Decay Plot","Exponential decay curve","Radioactive decay, clearance","Time + Quantity","no","—"),

    # ── 10. Enrichment & Pathway Analysis (30) ──────────────
    c("Enrichment & Pathway","GO Bar Plot","GO terms ranked by significance bar","GO enrichment results","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","GO Dot Plot","GO terms as sized + colored dots","GO enrichment multi-metric","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","GO Lollipop Plot","GO terms as lollipop","GO enrichment, ranked","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","GO Circular Plot","GO terms in a circle","GO enrichment, circular","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","KEGG Pathway Bar","KEGG pathway significance bar","KEGG enrichment results","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","KEGG Pathway Dot","KEGG as dot plot","KEGG multi-metric","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Enrichment Bubble","Count × −log10(p) bubble","Multi-metric enrichment overview","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Enrichment Heatmap","Terms × metrics tile","Multi-metric comparison across terms","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Gene-Concept Network (cnet)","Genes linked to terms in a network","Gene-term association","Enrichment + geneID","yes","Enrichment"),
    c("Enrichment & Pathway","Dot Sankey","Dot plot + Sankey flow","Pathway–gene flow","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","TF Circular Plot","Transcription factor target circles","TF enrichment","TF enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Pathway CC Plot","Pathway cluster comparison","Pathway comparison","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Enrichment Scatter","Count vs rich-factor scatter","Two-metric enrichment view","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Gene-Link Burden Bar","Genes shared across terms bar","Shared gene burden","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Radial Enrichment Bar","Polar bar of −log10(p)","Circular enrichment ranking","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Rich-Factor Bar","Terms ranked by rich factor","Rich-factor visualization","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Significance Bar (−log10p)","−log10(p-value) bar chart","Significance ranking","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Ontology Term Count Bar","GO subontology term frequency","GO namespace summary","GO enrichment","yes","Enrichment"),
    c("Enrichment & Pathway","Ontology Dot Summary","Dot per ontology class","GO namespace comparison","GO enrichment","yes","Enrichment"),
    c("Enrichment & Pathway","Term × Metric Tile","Tile heat per metric","Multi-metric summary view","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Term Scatter","Count vs −log10(p) scatter","Two-metric scatter","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","P-Value Histogram","p-value distribution histogram","Multiple testing diagnostics","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Gene Count Histogram","Gene count distribution","Gene burden per term","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Gene-Link Heatmap","Gene × term binary heatmap","Gene-term membership","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Polar Bubble Enrichment","Bubbles in polar layout","Circular enrichment view","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Waterfall Enrichment","Significance waterfall","Stepwise significance","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Ranked Line Profile","Terms as ranked line","Profile of enrichment","Enrichment data","yes","Enrichment"),
    c("Enrichment & Pathway","Ontology Stacked Metrics","Stacked bars per ontology","Cumulative metric by ontology","GO enrichment","yes","Enrichment"),
    c("Enrichment & Pathway","GSEA Enrichment Score Curve","Running enrichment score","Gene Set Enrichment Analysis","Ranked gene list","no","—"),
    c("Enrichment & Pathway","Enrichment Treemap","Terms sized by significance (treemap)","Hierarchical enrichment","Enrichment data","no","—"),

    # ── 11. Ranking & Comparison (25) ───────────────────────
    c("Ranking & Comparison","Sorted Bar Chart","Bars ordered by value","Ranking comparison","Cat + Num","yes","General Plots"),
    c("Ranking & Comparison","Ranking Lollipop","Ranked lollipop chart","Top-N ranked display","Cat + Num","yes","General Plots / Best Plots"),
    c("Ranking & Comparison","Dumbbell Ranking","Two endpoints per item connected","Before-after or two-group rank","Cat + 2 Num","yes","General Plots / Best Plots"),
    c("Ranking & Comparison","Slope Ranking","Lines between two time points","Rank changes between periods","Cat + Num (2 periods)","yes","General Plots"),
    c("Ranking & Comparison","Bump Chart","Lines tracking rank over multiple x-points","Competitive ranking over time","Cat + Rank + X","yes","General Plots"),
    c("Ranking & Comparison","Dot Matrix Rank","Grid of dots colored by rank group","Proportional rank display","Cat + Num","no","—"),
    c("Ranking & Comparison","Proportional Symbol Rank","Circles sized by value","Area-encoded ranking","Cat + Num","no","—"),
    c("Ranking & Comparison","Arrow Chart","Arrows showing direction + magnitude","Change direction emphasis","Cat + Start + End","no","—"),
    c("Ranking & Comparison","Tile Ranking","Grid tiles colored by rank","Matrix ranking view","Cat × Cat + Num","no","—"),
    c("Ranking & Comparison","Box Plot Ranking","Boxes ordered by median","Ranked distribution comparison","Cat + Num","no","—"),
    c("Ranking & Comparison","Strip Plot Ranking","Points ordered by value","Ranked individual values","Cat + Num","no","—"),
    c("Ranking & Comparison","Scatter Ranking","Scatter with rank on one axis","Value vs rank relationship","Num","no","—"),
    c("Ranking & Comparison","Labeled Ranking","Text-annotated ranked bars","Precise rank communication","Cat + Num","no","—"),
    c("Ranking & Comparison","Faceted Ranking","Ranked bars per panel","Multi-group ranking","Cat + Num + Facet","no","—"),
    c("Ranking & Comparison","Grouped Ranking","Category + sub-rank","Multi-level ranking","Cat + Subcat + Num","no","—"),
    c("Ranking & Comparison","Clustered Ranking","HC-ordered rank","Similarity-based rank","Cat + Num","no","—"),
    c("Ranking & Comparison","Comparative Ranking","Two sets ranked side-by-side","Contrast two rankings","Cat + 2 Num","no","—"),
    c("Ranking & Comparison","Radial Ranking","Ranked bars on polar axis","Circular ranking view","Cat + Num","no","—"),
    c("Ranking & Comparison","Heat Ranking","Heatmap ordered by rank","Matrix + rank combined","Cat × Cat + Num","no","—"),
    c("Ranking & Comparison","Waterfall Ranking","Cumulative contribution","Pareto-style attribution","Cat + Num","no","—"),
    c("Ranking & Comparison","Pareto Chart","Sorted bar + cumulative % line","80/20 rule analysis","Cat + Num","yes","General Plots"),
    c("Ranking & Comparison","Spine Ranking","Proportional bars ordered","Relative rank composition","Cat + Num","no","—"),
    c("Ranking & Comparison","Annotated Rank Chart","Rank with context notes","Story-telling rank","Cat + Num","no","—"),
    c("Ranking & Comparison","Target-Reference Rank","Rank relative to a target value","Performance vs standard","Cat + Num + Target","no","—"),
    c("Ranking & Comparison","Percentile Rank Chart","Percentile bands","Norm-referenced comparison","Cat + Num","no","—"),

    # ── 12. Multivariate Analysis (25) ──────────────────────
    c("Multivariate","Parallel Coordinates Plot","Each row as a polyline across variable axes","Multivariate pattern detection","Multi Num","yes","General Plots"),
    c("Multivariate","Radar / Spider Chart","Polygon per entity across radial axes","Profile comparison across variables","Cat + Multi Num","yes","General Plots / Best Plots"),
    c("Multivariate","PCA Biplot","PC1 vs PC2 with loadings","Variance structure in data","Multi Num","no","—"),
    c("Multivariate","Scatter Plot Matrix (SPLOM)","All-pairs scatter","Multivariate exploratory analysis","Multi Num","yes","General Plots"),
    c("Multivariate","Correlation Heatmap","Pairwise correlation matrix","Variable relationships","Multi Num","yes","General Plots"),
    c("Multivariate","Profile Chart","Line profiles per entity","Cross-variable entity comparison","Multi Num per entity","no","—"),
    c("Multivariate","Facet Grid","2D panel grid (rows × cols)","Two conditioning variables","Any + 2 Facet vars","yes","General Plots"),
    c("Multivariate","Facet Wrap","Wrapped panels per group","One conditioning variable","Any + Facet var","yes","General Plots"),
    c("Multivariate","Small Multiples","Array of mini charts","Compact multi-group display","Any + Group","no","—"),
    c("Multivariate","Trellis / Conditioned Plot","Scatterplot conditioned on 2 variables","Interaction visualization","Any + 2 Cond vars","no","—"),
    c("Multivariate","Bubble Matrix","Grid of bubbles (size + color)","Two-variable encoding in a matrix","Cat × Cat + 2 Num","no","—"),
    c("Multivariate","Chord Multivariate","Circular flows","Multi-group bidirectional flows","Multi Cat + Flow","no","—"),
    c("Multivariate","Star Plot","Radar per individual observation","Individual profile comparison","Multi Num per row","no","—"),
    c("Multivariate","Andrews Curves","Fourier-transformed parallel coords","Outlier detection in multi dimensions","Multi Num","no","—"),
    c("Multivariate","Chernoff Faces","Variables mapped to face features","Pattern detection (historical)","Multi Num","no","—"),
    c("Multivariate","Parallel Box Plots","Side-by-side boxes per variable","Multi-variable distribution","Multi Num","no","—"),
    c("Multivariate","Multi-Panel Line","Line chart per variable","Multi-variable trend","Multi Num × Time","no","—"),
    c("Multivariate","Ternary Plot","Three-component composition","Geochemistry, mixing proportions","3 Num (summing to 1)","no","—"),
    c("Multivariate","3D Scatter Plot","Three numeric axes","3D relationships","3 Num","no","—"),
    c("Multivariate","Heatmap + Dendrogram","Clustered heatmap with side dendrograms","Expression pattern with clustering","Multi Num","yes","General Plots"),
    c("Multivariate","Correlation Network","Variables as nodes, edges = |r|","Variable association as network","Multi Num","no","—"),
    c("Multivariate","Biplot with Ellipses","PCA + group ellipses","Group separation in PCA space","Multi Num + Group","no","—"),
    c("Multivariate","Scatter + Color + Size","Three-encoding scatter","Four-variable exploration","2 Num + Size + Color","yes","General Plots"),
    c("Multivariate","Multi-Metric Dashboard","Multiple stat summaries in one panel","Comprehensive variable overview","Multi Num","no","—"),
    c("Multivariate","Paired Scatter","Matched pairs as connected dots","Paired comparison of measurements","2 Num (paired)","no","—"),

    # ── 13. Time Series & Temporal (25) ─────────────────────
    c("Time Series","Time Series Line","Values over time as a line","Temporal trends","Time + Num","yes","General Plots"),
    c("Time Series","Time Series Area","Filled area chart over time","Magnitude + trend","Time + Num","yes","General Plots"),
    c("Time Series","Step Chart","Discrete jumps over time","State changes, discrete events","Time + Num","yes","General Plots"),
    c("Time Series","Ribbon / Band Chart","Uncertainty band over time","Forecast or CI range","Time + Lower + Upper","yes","General Plots"),
    c("Time Series","Calendar Heatmap","Day × week grid","Daily patterns (GitHub style)","Date + Num","yes","General Plots"),
    c("Time Series","Spiral Chart","Line on polar coordinates","Cyclical/periodic patterns","Time + Num","no","—"),
    c("Time Series","Gantt Chart","Task bars on timeline","Project scheduling","Task + Start + End","no","—"),
    c("Time Series","Timeline Chart","Events on horizontal axis","Event history or sequence","Event + Time","no","—"),
    c("Time Series","Streamgraph","Flowing stacked areas","Volume changes over time","Time + Cat + Num","no","—"),
    c("Time Series","Bump Chart","Rank lines over time","Competitive rank changes","Cat + Rank + Time","yes","General Plots"),
    c("Time Series","Candlestick Chart","OHLC bars","Financial/trading data","Time + Open + High + Low + Close","no","—"),
    c("Time Series","Horizon Chart","Compressed stacked area","Dense multi-series comparison","Time + Num","no","—"),
    c("Time Series","Dot Plot Over Time","Points on time axis","Sporadic time events","Time + Num","no","—"),
    c("Time Series","Duration Chart","Bars from start to end","Event durations","Event + Start + End","no","—"),
    c("Time Series","Cycle Plot","Repeating seasonal cycles","Seasonal decomposition","Time (seasonal) + Num","no","—"),
    c("Time Series","Forecasting Chart","Historical + predicted line","Future projection","Time + Num + Forecast","no","—"),
    c("Time Series","Anomaly Detection Plot","Time series with anomaly flags","Unusual event detection","Time + Num + Anomaly","no","—"),
    c("Time Series","Rolling Average Line","Smoothed moving average","De-noised trend","Time + Num","no","—"),
    c("Time Series","Event Rug Plot","Tick marks for events on timeline","Event frequency on time axis","Time + Event","no","—"),
    c("Time Series","Seasonal Decomposition Plot","Trend + seasonal + residual","Decompose time series components","Time + Num","no","—"),
    c("Time Series","Autocorrelation (ACF) Plot","Correlation at different lags","Time series dependency structure","Time + Num","no","—"),
    c("Time Series","Partial ACF Plot","Partial correlation at lags","ARIMA model diagnostics","Time + Num","no","—"),
    c("Time Series","Lag Plot","y[t] vs y[t−k]","Non-linearity and patterns","Time + Num","no","—"),
    c("Time Series","Waterfall Time Series","Cumulative changes over time","Contribution attribution","Time + Num","no","—"),
    c("Time Series","Multi-Series Facet Line","Line panels per series","Dense multi-series comparison","Time + Num + Group","yes","General Plots"),

    # ── 14. Uncertainty & Error (20) ────────────────────────
    c("Uncertainty & Error","Error Bar Plot","Mean ± SD/SE bars","Statistical comparison","Cat + Mean + Error","yes","General Plots"),
    c("Uncertainty & Error","Crossbar Plot","Box-style error with middle mark","Confidence interval display","Cat + Estimate + CI","no","—"),
    c("Uncertainty & Error","Linerange Plot","Vertical CI range","Simple interval","Cat + Lower + Upper","no","—"),
    c("Uncertainty & Error","Pointrange Plot","Point + range combined","Estimate with interval","Cat + Estimate + CI","yes","General Plots"),
    c("Uncertainty & Error","Confidence Ribbon","Shaded CI band on line","Trend uncertainty","Num + Lower + Upper","yes","General Plots"),
    c("Uncertainty & Error","Gradient Uncertainty Band","Faded CI away from estimate","Smooth uncertainty visualization","Num + Lower + Upper","no","—"),
    c("Uncertainty & Error","Violin (Posterior)","Distribution shape as violin","Bayesian posterior","Distribution","yes","General Plots"),
    c("Uncertainty & Error","Half-Eye Plot","Half density + CI interval","Bayesian uncertainty display","Distribution + CI","no","—"),
    c("Uncertainty & Error","Interval Chart","Horizontal CI intervals","Multiple parameter comparison","Cat + Estimate + CI","no","—"),
    c("Uncertainty & Error","Dots + Intervals","Dot cloud + CI","Full distribution + CI","Distribution","no","—"),
    c("Uncertainty & Error","Slab Chart","Density slab","Posterior density visualization","Distribution","no","—"),
    c("Uncertainty & Error","Raincloud with CI","Half violin + CI + dots","Full uncertainty picture","Distribution + CI","no","—"),
    c("Uncertainty & Error","Forest Plot (CI)","Effect sizes + intervals per study","Meta-analysis / systematic review","Cat + Estimate + CI","yes","General Plots"),
    c("Uncertainty & Error","Box IQR","Quartile uncertainty bars","Non-parametric interval","Cat + Num","yes","General Plots"),
    c("Uncertainty & Error","Bootstrap CI Plot","Resampled distribution + CI","Bootstrap inference visualization","Statistic + Bootstrap","no","—"),
    c("Uncertainty & Error","Bayesian Posterior Plot","Posterior density","Bayesian credible intervals","Posterior samples","no","—"),
    c("Uncertainty & Error","Prediction Interval","Model prediction + PI band","Regression forecasting","Num × Num + PI","no","—"),
    c("Uncertainty & Error","Profile with SE","Profile lines + SE ribbon","Longitudinal mean profiles","Time + Mean + SE","no","—"),
    c("Uncertainty & Error","Simulation Envelope","Simulated range band","Monte Carlo visualization","Sim + Num","no","—"),
    c("Uncertainty & Error","Gradient Error Bar","Error bar faded at extremes","Elegant uncertainty display","Cat + Mean + Error","no","—"),

    # ── 15. Correlation & Association (20) ──────────────────
    c("Correlation & Association","Correlation Scatter","Scatter with Pearson r","Two-variable linear relationship","2 Num","yes","General Plots"),
    c("Correlation & Association","Correlation Heatmap","Color-coded correlation matrix","All pairwise correlations","Multi Num","yes","General Plots"),
    c("Correlation & Association","Correlation Matrix Plot","Numbers + color-coding","Precise correlation values","Multi Num","yes","General Plots"),
    c("Correlation & Association","Correlation Network","Variables as nodes, r as edge weight","High-correlation variable clusters","Multi Num","no","—"),
    c("Correlation & Association","Correlation Bubble Plot","Circle size = |r|, color = sign","Compact correlation display","Multi Num","no","—"),
    c("Correlation & Association","Correlogram (ggcorrplot)","Full correlogram with method options","All correlation styles","Multi Num","no","—"),
    c("Correlation & Association","Ellipse Correlation","Angled ellipses representing r","Visual r encoding","Multi Num","no","—"),
    c("Correlation & Association","Circle Correlation","Filled circles (r = size + color)","Compact matrix","Multi Num","no","—"),
    c("Correlation & Association","Upper Triangle Correlation","Half correlation matrix","Symmetric matrix, half shown","Multi Num","no","—"),
    c("Correlation & Association","Lower Triangle Correlation","Lower half only","Symmetric, lower view","Multi Num","no","—"),
    c("Correlation & Association","Mixed Correlation Plot","Upper and lower with different styles","Comprehensive pair view","Multi Num","no","—"),
    c("Correlation & Association","Partial Correlation Plot","Correlation controlling for confounders","Causal pathway analysis","Multi Num","no","—"),
    c("Correlation & Association","Scatter Pair Matrix","All-pairs scatter + correlation","Comprehensive bivariate exploration","Multi Num","yes","General Plots"),
    c("Correlation & Association","Pair Plot (GGally)","Pairs with diagonal density","Full exploratory pair view","Multi Num","no","—"),
    c("Correlation & Association","Cross-Correlation Plot","Lagged correlations between series","Time series dependency","2 Time + Num","no","—"),
    c("Correlation & Association","Rank Correlation (Spearman)","Scatter with rank-based r","Monotone but non-linear relationship","2 Num","no","—"),
    c("Correlation & Association","Confidence Ellipse Scatter","Scatter + 95% CI ellipse","Group centroid and spread","2 Num + Group","yes","General Plots"),
    c("Correlation & Association","Correlation Bar Chart","Coefficients as horizontal bars","Rank correlations with reference","Multi Num + Target","no","—"),
    c("Correlation & Association","Correlation Dendrogram","HC tree of variable similarity","Cluster correlated variables","Multi Num","no","—"),
    c("Correlation & Association","Partial R² Barplot","Partial R² per predictor","Variable importance in regression","Predictor + R²","no","—"),

    # ── 16. Geographic & Spatial (15) ───────────────────────
    c("Geographic & Spatial","Choropleth Map","Regions filled by data value","Regional statistics","Region + Num","no","—"),
    c("Geographic & Spatial","Dot Map","Individual points on map","Point events or locations","Lat + Lon","no","—"),
    c("Geographic & Spatial","Bubble Map","Sized circles on geographic map","Spatial magnitude","Lat + Lon + Num","no","—"),
    c("Geographic & Spatial","Flow Map","Arrows/lines between locations","Migration, trade flows","Origin + Dest + Num","no","—"),
    c("Geographic & Spatial","Hexbin Map","Hexagonal spatial bins","Spatial density, large datasets","Lat + Lon","no","—"),
    c("Geographic & Spatial","Spatial Density Heat Map","Kernel density on map","Hotspot detection","Lat + Lon","no","—"),
    c("Geographic & Spatial","Cartogram","Area distorted to data value","Proportional region comparison","Region + Num","no","—"),
    c("Geographic & Spatial","Tile Map","Equal-area tiles per region","Fair spatial comparison","Region","no","—"),
    c("Geographic & Spatial","Connection Map","Lines between locations","Network or flow between places","Origin + Dest","no","—"),
    c("Geographic & Spatial","Graduated Symbol Map","Symbols sized by value","Spatial magnitude","Lat + Lon + Num","no","—"),
    c("Geographic & Spatial","Isopleth / Contour Map","Contour lines of equal value","Continuous spatial field","Lat + Lon + Num","no","—"),
    c("Geographic & Spatial","Dasymetric Map","Population-adjusted choropleth","Accurate spatial density","Region + Pop + Num","no","—"),
    c("Geographic & Spatial","Voronoi Map","Territory polygons from points","Area-of-influence visualization","Lat + Lon","no","—"),
    c("Geographic & Spatial","Centroid Map","Polygon centroids as points","Simplified spatial reference","Region polygons","no","—"),
    c("Geographic & Spatial","Proportional Symbol Map","Circle area ∝ value on map","Spatial magnitude comparison","Lat + Lon + Num","no","—"),

    # ── 17. Text & Word Visualization (15) ──────────────────
    c("Text & Word","Word Cloud","Words sized by frequency","Text frequency visualization","Text","no","—"),
    c("Text & Word","Word Tree","Branching phrase tree","Phrase usage in text","Text","no","—"),
    c("Text & Word","Word Frequency Bar","Top terms as bar chart","Most frequent words","Text","no","—"),
    c("Text & Word","Bigram Network","Word-pair co-occurrence network","Word association in text","Text","no","—"),
    c("Text & Word","Trigram Network","Three-word chain network","Phrase chain analysis","Text","no","—"),
    c("Text & Word","Sentiment Bar","Positive / negative sentiment","Text sentiment analysis","Text + Sentiment","no","—"),
    c("Text & Word","Topic Model Chart","Topic probability distribution","LDA topic model results","Document × Topic","no","—"),
    c("Text & Word","Text Timeline","Words / topics over time","Temporal text analysis","Time + Text","no","—"),
    c("Text & Word","Co-occurrence Heatmap","Word pair co-occurrence matrix","Semantic proximity","Term × Term","no","—"),
    c("Text & Word","TF-IDF Bar","Term importance bar","Document-specific terms","Doc + TF-IDF","no","—"),
    c("Text & Word","Term Frequency Distribution","Zipf's law histogram","Frequency distribution of terms","Text","no","—"),
    c("Text & Word","Collocation Network","Adjacent-word pair network","Phrase structure","Text","no","—"),
    c("Text & Word","Text Bubble","Proportional bubbles of terms","Keyword cloud alternative","Text + Freq","no","—"),
    c("Text & Word","Readability Chart","Text metrics (Flesch, Gunning Fog)","Document readability comparison","Text documents","no","—"),
    c("Text & Word","Document Similarity Heatmap","Cosine similarity between docs","Corpus similarity overview","Document × Document","no","—"),

    # ── 18. Molecular Docking & Drug Discovery (25) ─────────
    c("Molecular Docking","Docking Score Lollipop","Top molecules ranked by best score","Top binding molecule identification","Docking data","yes","Best Plots"),
    c("Molecular Docking","Docking Score Violin","Score distribution per fraction","Score spread by sample fraction","Docking data","yes","Best Plots"),
    c("Molecular Docking","Docking Score Bubble","Class × fraction bubble matrix","Multi-group score overview","Docking data","yes","Best Plots"),
    c("Molecular Docking","Docking Heatmap","Molecule × target score matrix","Binding affinity landscape","Docking data","yes","Docking Heatmap / Best Plots"),
    c("Molecular Docking","Molecular Radar","Target-class profile per molecule","Binding profile of a compound","Docking data","yes","Best Plots"),
    c("Molecular Docking","Hit-Rate Bar","% targets with score ≤ threshold","Compound potency metric","Docking data","yes","Best Plots"),
    c("Molecular Docking","ROMANID Dumbbell","Best vs mean per ROMANID group","Compound group comparison","Docking data","yes","Best Plots"),
    c("Molecular Docking","Protein Class Bar","Count of docked protein classes","Target diversity analysis","Docking data","yes","Classification"),
    c("Molecular Docking","Score ECDF","Cumulative score distribution","Percentile-based thresholds","Docking data","yes","Score Explorer"),
    c("Molecular Docking","Score Density by Class","Score density curves per class","Distribution comparison","Docking data","yes","Best Plots / Score Explorer"),
    c("Molecular Docking","Score Slope (Best vs Median)","Slope per molecule: best vs median","Consistency of binding","Docking data","yes","Best Plots"),
    c("Molecular Docking","Fraction Active Stack","Active / inactive stacked bar","Fraction hit rate","Docking data","yes","Best Plots"),
    c("Molecular Docking","Target Lollipop Profile","Targets ranked by score","Target binding strength","Docking data","yes","Best Plots"),
    c("Molecular Docking","Target Count Bar","Targets ranked by active-binding count","Target engagement frequency","Docking data","yes","Best Plots"),
    c("Molecular Docking","Score Waterfall","Incremental score changes","Score contribution attribution","Docking data","no","—"),
    c("Molecular Docking","Molecule Mean vs Best Scatter","Mean vs best score per molecule","Consistency vs potency","Docking data","yes","Best Plots"),
    c("Molecular Docking","Docking Tile Map","Molecule × target tile with color","Pairwise binding overview","Docking data","yes","Best Plots"),
    c("Molecular Docking","Radial Docking Bar","Polar bar of top molecules","Circular binding ranking","Docking data","yes","Best Plots"),
    c("Molecular Docking","Classification Fraction Heatmap","Fraction × class count heatmap","Class-fraction distribution","Docking data","yes","Classification"),
    c("Molecular Docking","Score Strip Plot","Raw score points per group","Show all individual scores","Docking data","yes","Classification"),
    c("Molecular Docking","Molecule–Target Binding Network","Bipartite molecule–target network","Network of binding interactions","Docking data","yes","TCM Network"),
    c("Molecular Docking","ADMET Radar Profile","Multi-property drug-likeness radar","Drug property overview","ADMET data","no","—"),
    c("Molecular Docking","Lead Compound Plot","Top scoring compounds panel","Hit identification summary","Docking data","no","—"),
    c("Molecular Docking","Docking Score Boxplot per Class","Boxplot by protein classification","Score distribution per class","Docking data","yes","Classification / Score Explorer"),
    c("Molecular Docking","Degree Plot (TCMNP)","Node degree bar/Cleveland dot","Network hub identification","Network data","yes","Degree Plots"),

    # ── 19. Specialized Bioinformatics (30) ─────────────────
    c("Specialized Bioinformatics","Gene Expression Heatmap","Log2FC heatmap of DEGs","Differential expression visualization","RNA-seq / microarray","no","—"),
    c("Specialized Bioinformatics","Volcano Plot","−log10(p) vs log2(FC)","Identify significant DEGs","DE results","no","—"),
    c("Specialized Bioinformatics","Manhattan Plot","Genomic position vs significance","GWAS significant loci","GWAS results","no","—"),
    c("Specialized Bioinformatics","Kaplan-Meier Survival Curve","Survival probability over time","Clinical survival analysis","Survival data","no","—"),
    c("Specialized Bioinformatics","ROC Curve","Sensitivity vs 1-Specificity","Biomarker / classifier evaluation","Score + Outcome","no","—"),
    c("Specialized Bioinformatics","PCA Sample Plot","Samples in PC1/PC2 space","Batch effects, clustering","Multi Num (omics)","no","—"),
    c("Specialized Bioinformatics","t-SNE Plot","Non-linear 2D embedding","Single-cell clustering","Multi Num (omics)","no","—"),
    c("Specialized Bioinformatics","UMAP Plot","Manifold dimensionality reduction","Single-cell cell types","Multi Num (omics)","no","—"),
    c("Specialized Bioinformatics","Sequence Logo","Position-weight matrix","Motif visualization","Aligned sequences","no","—"),
    c("Specialized Bioinformatics","Phylogenetic Tree","Evolutionary relationships","Phylogenetics","Phylogenetic tree","no","—"),
    c("Specialized Bioinformatics","Circos Plot","Circular genome visualization","Structural variants, gene fusion","Genomic","no","—"),
    c("Specialized Bioinformatics","Mutation Lollipop","Protein position × mutation count","Mutational hotspot identification","Mutation data","no","—"),
    c("Specialized Bioinformatics","OncoPrint / Alteration Matrix","Sample × gene alteration heatmap","Cancer genomics multi-omics","Alteration matrix","no","—"),
    c("Specialized Bioinformatics","Copy Number Variation Profile","Genomic CNV log-ratio","Chromosomal gains/losses","CNV data","no","—"),
    c("Specialized Bioinformatics","Genome Browser Track","Read depth or signal track","ChIP-seq, ATAC-seq","Genomic signal","no","—"),
    c("Specialized Bioinformatics","Motif Occurrence Heatmap","Motif position frequency","Regulatory motif analysis","Motif data","no","—"),
    c("Specialized Bioinformatics","Gene Expression Violin","Violin per gene or sample","Single-gene expression comparison","Expression matrix","no","—"),
    c("Specialized Bioinformatics","Pathway Map Visualization","Pathway diagram with overlaid data","KEGG/Reactome pathway overlay","Pathway + expression","no","—"),
    c("Specialized Bioinformatics","Co-expression Network","Genes as nodes, r > threshold","Gene module detection","Expression matrix","no","—"),
    c("Specialized Bioinformatics","Chromosome Ideogram","Karyotype with annotations","Genomic feature location","Chromosomal positions","no","—"),
    c("Specialized Bioinformatics","SNP Density Plot","Variant count per genomic window","Population genetics","VCF / SNP data","no","—"),
    c("Specialized Bioinformatics","LD Decay Plot","Linkage disequilibrium vs distance","Population structure","LD data","no","—"),
    c("Specialized Bioinformatics","Differential Expression Bar","Upregulated/downregulated bar","DEG count summary","DE results","no","—"),
    c("Specialized Bioinformatics","Expression QC Box Plot","Box per sample (library QC)","RNA-seq quality control","Expression matrix","no","—"),
    c("Specialized Bioinformatics","GSEA Running Enrichment Score","Enrichment score curve","Gene Set Enrichment Analysis","Ranked gene list","no","—"),
    c("Specialized Bioinformatics","Clonotype Frequency Plot","TCR/BCR clonotype distribution","Immune repertoire analysis","Clonotype data","no","—"),
    c("Specialized Bioinformatics","Cell-Type Proportion Bar","Cell type % per sample","Single-cell composition","Cell metadata","no","—"),
    c("Specialized Bioinformatics","Feature Plot (scRNA)","Gene expression in 2D embedding","Single-cell gene visualization","scRNA-seq","no","—"),
    c("Specialized Bioinformatics","Dot Plot (scRNA)","Gene × cluster dot plot","Single-cell marker genes","scRNA-seq","no","—"),
    c("Specialized Bioinformatics","Trajectory Plot","Cell differentiation trajectory","Pseudotime analysis","scRNA-seq + PT","no","—"),

    # ── 20. Composite & Dashboard (20) ──────────────────────
    c("Composite & Dashboard","Multi-Plot Panel","Grid of multiple charts","Overview dashboard","Any + Multiple","no","—"),
    c("Composite & Dashboard","Inset Chart","Small chart embedded in main chart","Zoom or supplementary detail","Any","no","—"),
    c("Composite & Dashboard","Sparkline","Tiny trend line in text/table","Compact inline trend","Time + Num","no","—"),
    c("Composite & Dashboard","KPI Metric Card","Single large number with trend","Key performance indicator","Single Num","no","—"),
    c("Composite & Dashboard","Gauge / Speedometer Chart","Dial-style KPI","Single metric vs goal","Single Num + Target","no","—"),
    c("Composite & Dashboard","Bullet Chart","Bar + comparative + target","KPI with context","Cat + Num + Target","no","—"),
    c("Composite & Dashboard","Progress Bar Chart","Horizontal progress bar","Completion / progress","Cat + Pct","no","—"),
    c("Composite & Dashboard","Dashboard Tile Panel","Grid of summary metrics","Executive summary","Multiple KPIs","no","—"),
    c("Composite & Dashboard","Side-by-Side Comparison","Two charts aligned","A/B comparison","Any × 2","no","—"),
    c("Composite & Dashboard","Narrative / Story Chart","Annotated story-driven chart","Data storytelling","Any","no","—"),
    c("Composite & Dashboard","Annotated Dashboard","Rich-annotation composite","Report-ready composite","Any","no","—"),
    c("Composite & Dashboard","Themed Composite","Brand-styled multi-chart","Corporate reporting","Any","no","—"),
    c("Composite & Dashboard","Patchwork Layout","Flexible ggplot panel layout","Custom publication figure","Multiple ggplots","no","—"),
    c("Composite & Dashboard","Summary Statistics + Chart","Table + visualization combined","Descriptive + visual","Any","no","—"),
    c("Composite & Dashboard","Combined Distribution","Multiple distribution overlays","Full distributional comparison","Multiple Num","no","—"),
    c("Composite & Dashboard","Before/After Comparison","Two states shown together","Intervention or change effect","2 States + Num","no","—"),
    c("Composite & Dashboard","Performance Matrix","Multiple KPI panel","Balanced scorecard","Multiple KPIs","no","—"),
    c("Composite & Dashboard","Correlation + Scatter Combo","Heatmap + scatter in one panel","Relationship + distribution","Multi Num","no","—"),
    c("Composite & Dashboard","Time + Distribution Combo","Time series + distribution panel","Temporal + statistical summary","Time + Num","no","—"),
    c("Composite & Dashboard","Hierarchy + Distribution Combo","Tree + violin in one panel","Network + distribution combo","Hierarchy + Num","no","—")
  )
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  colnames(df) <- c("Category","Name","Description","Best Use Case","Data Requirements","Implemented","Available In")
  df$`#` <- seq_len(nrow(df))
  df[, c("#","Category","Name","Description","Best Use Case","Data Requirements","Implemented","Available In")]
}

GALLERY_DATA <- .make_gallery_data()


# ── UI ───────────────────────────────────────────────────────

#' UI for the Plot Classification Gallery tab.
#' @return A `shinydashboard::tabItem` object.
galleryUI <- function() {
  tabItem("gallery",
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = paste0("\U0001f4ca Plot Classification Gallery \u2014 ",
                         nrow(GALLERY_DATA), " Chart Types in 20 Categories"),
        fluidRow(
          column(4,
            selectInput("gallery_cat",
              "Filter by Category",
              choices = c("All Categories" = "",
                          sort(unique(GALLERY_DATA$Category))),
              selected = "")
          ),
          column(4,
            selectInput("gallery_impl",
              "Filter by Implementation Status",
              choices = c("All" = "",
                          "Implemented in this app" = "yes",
                          "Reference / roadmap"     = "no"),
              selected = "")
          ),
          column(4,
            p(style = "margin-top:10px;",
              em(paste0("LekhaChitralya implements ",
                        sum(GALLERY_DATA$Implemented == "yes"),
                        " of ", nrow(GALLERY_DATA),
                        " catalogued chart types across all tabs."))
            )
          )
        ),
        hr(),
        DT::dataTableOutput("gallery_table")
      )
    )
  )
}


# ── Server ───────────────────────────────────────────────────

#' Server logic for the Plot Classification Gallery tab.
#' @param input   Shiny `input` object.
#' @param output  Shiny `output` object.
#' @param session Shiny `session` object.
#' @param rv      Shared `reactiveValues` (unused; kept for consistency).
galleryServer <- function(input, output, session, rv) {

  # Category colours (one per unique category)
  cat_levels <- sort(unique(GALLERY_DATA$Category))
  cat_palette <- c(
    "#d4efdf","#d6eaf8","#fef9e7","#f9ebea","#f5eef8",
    "#e8f4f8","#fdf2e9","#eaf2ff","#fdfefe","#f8f9fa",
    "#fff3cd","#d1ecf1","#d4edda","#f8d7da","#e2e3e5",
    "#d6eaf8","#a9cce3","#a9dfbf","#f9e4b7","#e8daef"
  )
  cat_cols <- setNames(
    rep_len(cat_palette, length(cat_levels)),
    cat_levels
  )

  filtered <- reactive({
    d <- GALLERY_DATA
    cat_sel  <- input$gallery_cat
    impl_sel <- input$gallery_impl
    if (!is.null(cat_sel)  && nzchar(cat_sel))
      d <- d[d$Category == cat_sel, ]
    if (!is.null(impl_sel) && nzchar(impl_sel))
      d <- d[d$Implemented == impl_sel, ]
    d
  })

  output$gallery_table <- DT::renderDataTable({
    df <- filtered()
    # Render Implemented column as symbols
    df$Implemented <- ifelse(
      df$Implemented == "yes",
      "<span style='color:#27ae60;font-weight:bold;'>&#10003; Yes</span>",
      "<span style='color:#aaa;'>&#8212; Ref</span>"
    )
    DT::datatable(
      df,
      filter    = "top",
      rownames  = FALSE,
      escape    = FALSE,
      options   = list(
        pageLength  = 25,
        scrollX     = TRUE,
        autoWidth   = FALSE,
        columnDefs  = list(
          list(width = "30px",  targets = 0),   # #
          list(width = "140px", targets = 1),   # Category
          list(width = "190px", targets = 2),   # Name
          list(width = "240px", targets = 3),   # Description
          list(width = "200px", targets = 4),   # Use Case
          list(width = "140px", targets = 5),   # Data
          list(width = "90px",  targets = 6),   # Implemented
          list(width = "130px", targets = 7)    # Available In
        )
      )
    ) %>%
      DT::formatStyle(
        "Category",
        backgroundColor = DT::styleEqual(names(cat_cols), unname(cat_cols))
      ) %>%
      DT::formatStyle(
        "Implemented",
        backgroundColor = DT::styleEqual(
          c("<span style='color:#27ae60;font-weight:bold;'>&#10003; Yes</span>",
            "<span style='color:#aaa;'>&#8212; Ref</span>"),
          c("#eafaf1", "#f8f9fa")
        )
      )
  })
}
