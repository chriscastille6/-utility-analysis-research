library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# goal setting and performance
grViz("
  digraph path_diagram {
    graph [rankdir = LR]
    node [shape = box, style = filled, fillcolor = lightblue]

    GoalSetting [label = 'Goal Setting']
    Performance [label = 'Job Performance']
    Profits [label = 'Profits']

    GoalSetting -> Performance [label = '+']
    Performance -> Profits [label = '+']
  }
")

# utility
grViz("
  digraph path_diagram {
    graph [layout = dot, rankdir = LR]

    node [
      shape = box,
      style = filled,
      fillcolor = lightblue,
      fontname = Helvetica,
      fontcolor = black,
      width = 2,
      fixedsize = true
    ]

    Quantity [label = 'Quantity']
    Quality [label = 'Quality']
    Cost [label = 'Cost']
    Utility [label = 'Utility']

    Quantity -> Utility
    Quality  -> Utility
    Cost     -> Utility
  }
")
