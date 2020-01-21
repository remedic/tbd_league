#https://cran.r-project.org/web/packages/elo/vignettes/elo.html

library(elo)

data(tournament)

tournament$wins.A <- tournament$points.Home > tournament$points.Visitor
ex1<-elo.run(wins.A ~ team.Home + team.Visitor, data = tournament, k = 20)

ex2<-elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor, data = tournament, k = 20)

ex3<-elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor +
          k(20*log(abs(points.Home - points.Visitor) + 1)), data = tournament)