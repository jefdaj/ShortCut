xvar = 1
yvar = 1200 * xvar
named = score_repeats yvar
                      xvar
                      [1, 2, 3, 4, 5, 2, 2, 4, 1, 3, 1, 1, 1, 1]
plots = [linegraph "yvars for a list of xvar values" named,
         linegraph "same but with unnamed (inline) yvar"
                   (score_repeats (1200 * xvar) xvar
                   [1, 2, 3, 4, 5, 2, 2, 4, 1, 3, 1, 1, 1, 1])]
result = plots
