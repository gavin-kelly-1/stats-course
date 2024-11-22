library(tidyverse)
library(ggwordcloud)
library(ragg)
library(ggmosaic)
library(jsonlite)
library(grid)
library(ggdag)
library(plotly)
library(htmlwidgets)
library(mvtnorm)

FCICOL <- c(
  red="#e30613", navy="#4565ad",
  blue = "#94cef2", yellow = "#ffe70b", green = "#76b82a", orange = "#f7ac6f", purple="#bc90c1",
  blue2 = "#bee2e9",yellow2 = "#fff6a7",green2 = "#acd084",orange2= "#ffe7ac",purple2="#fbdbdb",
  white = "#ffffff", black = "#000000", grey = "#706f6f"
)


              

save_plot <- function(name, expr, width=800, height=800, type="", prefix="images/r-",...) {
  types <- list(background=list(w=1920, h=1080), small_square=list(w=240, h=240))
  par_og <- par(no.readonly = TRUE) 
  if (type %in% names(types)) {
    width <- types[[type]]$w
    height <- types[[type]]$h
  }
  agg_png(file=paste0(prefix, name, ".png"),
          width=width, height=height,...)
  print(eval(expr))
  dev.off()
  par(par_og)
}


################################################################
"populations" %>%
  save_plot( {
    set.seed(7)
    par(mar = rep(0, 4))
    plot(c(0, 2), c(0, 1.1), type = "n", axes = FALSE, xlab = "", ylab = "")
    theta <- seq(0, 2 * pi, 2 * pi / 100)
    x <- 0.5 + 0.5 * cos(theta)
    y <- 0.5 + 0.5 * sin(theta)
    lines(x, y)
    random_points <- matrix(runif(1000), ncol = 2)
    is_pop <- apply((random_points-.5) ^2, 1, sum) < 0.23
    pop <- random_points[is_pop,]
    points(pop, col = FCICOL["navy"], pch = 20)
    text(0.5, 1, "All Mice", pos = 3, cex = 2)
    our_sample <- sample(nrow(pop), 10)
    lines((x - 0.5) / 2 + 1.5,
    (y - 0.5) / 2 + 0.5,
    pch = 20
    )
    SS <- (pop[our_sample, ] - 0.5) / 2 + 0.5
    points(SS[, 1] + 1, SS[, 2], col = FCICOL["navy"], pch = 20, cex = 1.5)
    text(1.5, 0.75, "Sample", pos = 3, cex = 2)
    for (i in seq(along=our_sample)) {
      arrows(pop[our_sample[i], 1], pop[our_sample[i], 2],
             SS[i, 1] + 1 - 0.03, SS[i, 2],
             length = 0.08,
             col = FCICOL["black"],
             lwd = 1.5
             )
    }
  })


################################################################
"sub-populations" %>%
  save_plot( {
    set.seed(7)
    par(mar = rep(0, 4))
    plot(c(0, 2), c(0, 1.1), type = "n", axes = FALSE, xlab = "", ylab = "")
    theta <- seq(0, 2 * pi, 2 * pi / 100)
    x <- 0.5 + 0.5 * cos(theta)
    y <- 0.5 + 0.5 * sin(theta)
    lines(x, y)
    random_points <- matrix(runif(1000), ncol = 2)
    is_pop <- apply((random_points-.5) ^2, 1, sum) < 0.23
    pop <- random_points[is_pop,]
    is_subpop <- apply((t(t(pop)-c(0.55, 0.68))) ^2, 1, sum) < 0.07
    points(pop, col = FCICOL[ifelse(is_subpop, "navy", "blue")], pch = 20)
    text(0.5, 1, "All Mice", pos = 3, cex = 2)
    lines(
    (x - 0.5) * 2 * sqrt(0.07) + 0.55,
    (y - 0.5) * 2 * sqrt(0.07) + 0.68
    )
    our_sample <- sample(which(is_subpop), 10)
    lines((x - 0.5) / 2 + 1.5,
    (y - 0.5) / 2 + 0.5,
    pch = 20
    )
    SS <- (pop[our_sample, ] - 0.5) / 2 + 0.5
    points(SS[, 1] + 1, SS[, 2], col = FCICOL["navy"], pch = 20, cex = 1.5)
    text(1.5, 0.75, "Sample", pos = 3, cex = 2)
    for (i in seq(along=our_sample)) {
      arrows(pop[our_sample[i], 1], pop[our_sample[i], 2],
             SS[i, 1] + 1 - 0.03, SS[i, 2],
             length = 0.08,
             col = FCICOL["black"],
             lwd = 1.5
             )
    }
    text(0.55, 0.5 + 0.18 - sqrt(0.07),
         "Crick mice now",
         pos = 1, cex = 2
         )
  })

################################################################
"stratified-populations" %>%
  save_plot( {
    set.seed(7)
    par(mar = rep(0, 4))
    plot(c(0, 2), c(0, 1.1), type = "n", axes = FALSE, xlab = "", ylab = "")
    theta <- seq(0, 2 * pi, 2 * pi / 100)
    x <- 0.5 + 0.5 * cos(theta)
    y <- 0.5 + 0.5 * sin(theta)
    lines(x, y)
    random_points <- matrix(runif(1000), ncol = 2)
    is_pop <- apply((random_points-.5) ^2, 1, sum) < 0.23
    pop <- random_points[is_pop,]
    is_subpop <- apply((t(t(pop)-c(0.55, 0.68))) ^2, 1, sum) < 0.07
    subpop_a <- is_subpop & pop[,2]<0.85
    points(pop, col = FCICOL[ifelse(is_subpop, ifelse(subpop_a, "navy", "orange"),  "blue")], pch = 20)
    text(0.5, 1, "All Mice", pos = 3, cex = 2)
    lines(
    (x - 0.5) * 2 * sqrt(0.07) + 0.55,
    (y - 0.5) * 2 * sqrt(0.07) + 0.68
    )
    our_sample <- c(sample(which(is_subpop & subpop_a), 5),
                   sample(which(is_subpop & !subpop_a), 5))
    lines((x - 0.5) / 2 + 1.5,
    (y - 0.5) / 2 + 0.5,
    pch = 20
    )
    SS <- (pop[our_sample, ] - 0.5) / 2 + 0.5
    points(SS[, 1] + 1, SS[, 2], col = FCICOL[rep(c("navy","orange"), each=5)], pch = 20, cex = 1.5)
    text(1.5, 0.75, "Stratified\nSample", pos = 3, cex = 2)
    for (i in seq(along=our_sample)) {
      arrows(pop[our_sample[i], 1], pop[our_sample[i], 2],
             SS[i, 1] + 1 - 0.03, SS[i, 2],
             length = 0.08,
             col = FCICOL["black"],
             lwd = 1.5
             )
    }
    text(0.55, 0.5 + 0.18 - sqrt(0.07),
         "Crick mice now",
         pos = 1, cex = 2
         )
  })

################################################################
"overall-wordcloud" %>%
save_plot( {
  words <- c("Inference", "Treatment", "Experimental Unit", "Sample",
            "Randomisation", "Covariate", "Outcome", "Replication", "Pseudoreplication",
            "Standard Error", "Confidence Interval", "P-Value", "Association",
            "Hypothesis Test", "Confounding", "Bias", "Descriptive", "Visualisation",
            "Pairing", "Selection", "Data Types", "Censoring", "Missing Data",
            "Measurement Error", "Observational", "Power", "Design", "Validition",
            "Sparse Data", "Population", "Interactions", "Table II", "Models",
            "Control", "Independence")
  
  
  df <- data.frame(word=words) %>%
    mutate(angle = 45 * rnorm(n(), sd=.5),
           color = factor(sample.int(10, n(), replace = TRUE))
           )
  ggplot(df, aes(label = word, angle=angle, color=color)) +
    geom_text_wordcloud(size=9, family="TeX Gyre Heros") +
    theme_minimal()
  })


################################################################
## dag {
## bb="-1,-1,1,1"
## #		 Covariate [pos="0.000,-0.500"]
## Environment [pos="0.000,0.500"]
## Response [outcome,pos="0.500,0.000"]
## Treatment [exposure,pos="-0.500,0.000"]
## #		 Covariate -> Response
## Treatment -> Response
## }
local({

  dags <- list(
    dag1 = list(Response~Treatment,
              Environment~Environment),
    dag2 = list(Response~Treatment,
            Response~Environment),
    dag3 =  list(Response~Treatment,
               Response~Environment,
               Treatment~Environment),
    'dag-random' = list(Response~Treatment,
                      Response~Environment,
                      Treatment~Environment,
                      Response ~ Covariate),
    'dag-confounder' = list(Response~Treatment,
                       Response~Environment,
                       Treatment~Environment,
                       Response ~ Covariate,
                       Response ~ Confounder,
                       Treatment ~ Confounder),
    'dag-mediator' = list(Response~Treatment,
                       Response~Environment,
                       Treatment~Environment,
                       Response ~ Covariate,
                       Mediator ~ Treatment,
                       Response ~ Mediator),
    'dag-collider' = list(Response~Treatment,
                       Response~Environment,
                       Treatment~Environment,
                       Response ~ Covariate,
                       Collider ~ Treatment,
                       Collider ~ Response),
    'dag-complete' = list(Response~Treatment,
                       Response~Environment,
                       Treatment~Environment,
                       Response ~ Covariate,
                       Response ~ Confounder,
                       Treatment ~ Confounder,
                       Mediator ~ Treatment,
                       Response ~ Mediator,
                       Collider ~ Treatment,
                       Collider ~ Response)
  )
  
  dagifs <- lapply(dags, function(d) do.call(dagify,
                                     c(d, list(
                                       labels=c(Response="Response", Treatment="Treatment", Environment="Environment", Covariate="Covariate", Confounder="Confounder", Collider="Collider", Mediator="Mediator"),
                                       coords=list(
                                         x=c(Treatment=0, Response=1, Environment=0.5, Covariate=0.9, Confounder=0.5, Collider=0.5, Mediator=0.5),
                                         y=c(Treatment=0, Response=0, Environment=-0.5, Covariate=0.7, Confounder=0.5, Collider=0.9, Mediator=0.1)
                                       )
                                     )
                                     )
                                     )
                  )
  
  pl <- lapply(dagifs, function(d) ggplot(tidy_dagitty(d), aes(x=x, y=y, xend=xend, yend=yend)) +
    geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(20, "pt"), type = "closed")) + 
    geom_dag_node(size = 16) +
    geom_label(aes(label=label,
                   x=x+ifelse(label=="Treatment", -0.1, ifelse(label=="Response", 0.1,0.1)),
                   y=ifelse(label=="Environment", y-0.2, y)),
               nudge_y=.1,
               size = 5, col = "black", show.legend = FALSE) +
    ylim(-1,1) +
      theme_void()
    )
  
  "dag1" %>%
    save_plot({
      pl[[1]] 
    },
    scaling=1.5)
  "dag2" %>%
    save_plot({
      pl[[2]] +geom_label(aes(label=label), data=data.frame(x=c(.75), y=c(-0.25), xend=c(0.75), yend=c(-0.25), label=c("NOISE")))
    },
    scaling=1.5)
  "dag3" %>%
    save_plot({
      pl[[3]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)
  "dag-random" %>%
    save_plot({
      pl[[4]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)
  "dag-confounder" %>%
    save_plot({
      pl[[5]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)
  "dag-mediator" %>%
    save_plot({
      pl[[6]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)
    "dag-collider" %>%
    save_plot({
      pl[[7]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)
  "dag-complete" %>%
    save_plot({
      pl[[8]] +geom_label(aes(label=label), data=data.frame(x=c(.25,.75), y=c(-0.25,-0.25), xend=c(0.25,0.75), yend=c(-0.25,-0.25), label=c("RANDOM\nALLOCATION","NOISE")))
    },
    scaling=1.5)

})


local({
  set.seed(1)
  df <- expand.grid(Fraction=c("Nuclear", "Cytoplasm"), individual=factor(1:2), Condition=c("Control","Disease"))
  df$EG1 <- c(40,45, 65,60, 30,90, 40,95)
  "ziff-multivariate" %>%
    save_plot({
      df %>%
        pivot_wider(id_cols=c(individual,Condition), names_from=Fraction, values_from=EG1) %>%
        ggplot(aes(x=Nuclear, y=Cytoplasm, colour=Condition)) +
        geom_point(size=3) +
        xlim(c(20,70)) + ylim(c(40,110)) +
        theme_bw() + coord_fixed()
    }, scaling=3)
  
  angles <- (0:51) * 2 * pi/51
  unit.circle <- cbind(cos(angles), sin(angles))
  ellipse <- 10 * unit.circle %*% matrix(c(1,.6,.6,1),2,2)
  colnames(ellipse) <- c("x","y")
  
  e_frame <- df %>% group_by(Fraction, Condition) %>%
    summarise(EG1=mean(EG1)) %>%
    pivot_wider(id_cols=Condition, names_from=Fraction, values_from=EG1)
  ind <- expand.grid(i=1:2, e=1:nrow(unit.circle))
  e_frame <- cbind(e_frame[ind$i,], ellipse[ind$e,]) %>%
    mutate(Nuclear = Nuclear + x, Cytoplasm=Cytoplasm + y)
  df2 <- df %>%
    pivot_wider(id_cols=c(individual,Condition), names_from=Fraction, values_from=EG1)
  "ziff-multivariate-poscor" %>%
    save_plot({
      ggplot(df2, aes(x=Nuclear, y=Cytoplasm, colour=Condition)) +
        geom_point(size=3) +
        geom_path(data=e_frame) +
        xlim(c(20,70)) + ylim(c(40,110)) +
        theme_bw() + coord_fixed()
    } , scaling=3)

  ellipse <- 10 * unit.circle %*% matrix(c(1,-.6, -.6, 1),2,2)
  colnames(ellipse) <- c("x","y")
  
  e_frame <- df %>% group_by(Fraction, Condition) %>%
    summarise(EG1=mean(EG1)) %>%
    pivot_wider(id_cols=Condition, names_from=Fraction, values_from=EG1)
  ind <- expand.grid(i=1:2, e=1:nrow(unit.circle))
  e_frame <- cbind(e_frame[ind$i,], ellipse[ind$e,]) %>%
    mutate(Nuclear = Nuclear + x, Cytoplasm=Cytoplasm + y)
  df2 <- df %>%
    pivot_wider(id_cols=c(individual,Condition), names_from=Fraction, values_from=EG1)
  "ziff-multivariate-negcor" %>%
    save_plot({
      ggplot(df2, aes(x=Nuclear, y=Cytoplasm, colour=Condition)) +
        geom_point(size=3) +
        geom_path(data=e_frame) +
        xlim(c(20,70)) + ylim(c(40,110)) +
        theme_bw() + coord_fixed()
    }, scaling=3)
  
})


local({
  set.seed(1)
  ziff1 <- expand.grid(panel=factor(paste("Scenario", 1:3)),
                      fraction=c("Nucleus", "Cytoplasm"), disease=c("Disease", "Control"),
                      replicate=1:4, EG1=0)
  ziff1 <- mutate(ziff1,
                 EG1 = rnorm(nrow(ziff1),0,.1) +ifelse(disease=="Disease", 1,0),
                 EG1 = EG1+ifelse(panel=="Scenario 1" & fraction=="Nucleus", ifelse(disease=="Control", .1,0), 0),
                 EG1 = EG1+ifelse(panel=="Scenario 2" & fraction=="Nucleus", ifelse(disease=="Control", .3,-0.2), 0),
                 EG1 = EG1+ifelse(panel=="Scenario 3" & fraction=="Nucleus", ifelse(disease=="Control", .3,0), ifelse(disease=="Control", 0, -0.8))
                 )
  pl <- ggplot(ziff1, aes(x=fraction, y=EG1, colour=disease, group=interaction(replicate, disease))) +
    geom_point() + geom_line() +
    facet_grid(disease ~ panel) +
    scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Disease","Control"))) + 
    theme_bw()
  "temptation-stratify-3" %>%
    save_plot({pl
    },
    width=1200, height=400, scaling=2
    )
  g <- ggplotGrob(pl)
  g$grobs[[which(g$layout$name == "panel-1-3")]] = nullGrob()
  g$grobs[[which(g$layout$name == "panel-2-3")]] = nullGrob()
  g$grobs[[which(g$layout$name == "strip-t-3")]] = nullGrob()
  g$grobs[[which(g$layout$name == "axis-b-3")]] = nullGrob()
  "temptation-stratify-2" %>%
    save_plot({grid.draw(g)},
              width=1200, height=400, scaling=2
              )
  g$grobs[[which(g$layout$name == "panel-1-2")]] = nullGrob()
  g$grobs[[which(g$layout$name == "panel-2-2")]] = nullGrob()
  g$grobs[[which(g$layout$name == "strip-t-2")]] = nullGrob()
  g$grobs[[which(g$layout$name == "axis-b-2")]] = nullGrob()
  "temptation-stratify-1" %>%
    save_plot({grid.draw(g)},
              width=1200, height=400, scaling=2
              )


"temptation-stratify" %>%
  save_plot( {
    ggplot(ziff1, aes(x=fraction, y=EG1, colour=disease, group=interaction(replicate, disease))) +
      geom_point() + geom_line() +
      facet_grid(disease ~ panel) +
      scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Disease","Control"))) + 
      theme_bw()
  },
  width=1200, height=600, scaling=2
  )


"temptation-normalise" %>%
  save_plot( {
    ziff1 %>%
      dplyr::filter(panel=="Scenario 1") %>%
      dplyr::group_by(replicate, disease) %>%
      mutate(EG1=EG1+2,
             EG1=EG1/EG1[fraction=="Nucleus"]) %>%
      ungroup() %>%
      ggplot(aes(x=fraction, y=EG1, fill=disease)) +
      stat_summary(width = .8, geom = "bar", fun = "mean", position="dodge2") +
      stat_summary(aes(width = .8), lwd=1, geom = "errorbar", fun.data = "mean_se", position="dodge2") +
      scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("Disease","Control"))) +
      labs(y="Relative EG1") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      theme_bw()
  },
  width=800, height=800, scaling=3
  )
  
}) # end of ziff1's local env


local({
      set.seed(1)
      gen_data <- function(baseline, ctrl_outcome, treat_outcome, sd, rho, N, ind) {
        df <- expand.grid(baseline=1:N, group=c("Control","Disease"), outcome=0)
        rdat <- MASS::mvrnorm(2*N, mu=c(baseline, 0), matrix(c(1,rho,rho,1)*sd,2,2))
        df$baseline <- rdat[,1]
        df$outcome <- rdat[,2] + ifelse(df$group=="Control", ctrl_outcome, treat_outcome)
        types <- c("Just Cyto","Cyto:Nuclear","ANCOVA")
        res <- data.frame(type=types, lower=0, upper=0, baseline=diff(tapply(df$baseline, df$group, mean)), ind=ind, row.names=types)
        res["Just Cyto", c("lower", "upper")]  <- confint(lm(outcome~group, data=df))[2,]
        res["Cyto:Nuclear", c("lower", "upper")] <- confint(lm(outcome-baseline ~ group, data=df))[2,]
        res["ANCOVA", c("lower", "upper")] <- confint(lm(outcome ~ group + baseline , data=df))[2,]
        df2 <- rbind(
          data.frame(ind=factor(1:nrow(df)), group=df$group, y=df$outcome, var="outcome"),
          data.frame(ind=factor(1:nrow(df)), group=df$group, y=df$baseline, var="baseline"))
#        res["ANOVA", c("lower","upper")] <- confint(lm(y~group*var + ind, data=df2))["groupDisease:varoutcome",]
        res
      }
      
      resList <- lapply(1:1000, function(ind) gen_data(baseline=2200, ctrl_outcome=2200, treat_outcome=2400, sd=150, rho=0.6, N=50, ind=ind))
      res <- do.call(rbind, resList)
      res$CI <- ifelse(res$lower>200 | res$upper < 200, "Unlucky", "OK")
      res <- res[order(res$CI),]
      res$type <- factor(res$type, levels=c("Just Cyto", "Cyto:Nuclear", "ANCOVA"))
      pl <- ggplot(res,
                  aes(x=baseline, xend=baseline, y=lower, yend=upper, group=ind, colour=CI)) +
        geom_segment(lwd=.5) +
        scale_colour_manual(values=setNames(FCICOL[c("red", "blue")], c("Unlucky","OK"))) + 
        theme_bw() + theme(axis.text.y=element_blank()) +
        labs(x="Difference in nucleus", y="Effect Estimate", colour="Confidence\nInterval") +
        facet_grid(~type)
      
      "regression-to-mean-3" %>%
        save_plot({
          pl
        },
        width=1200, height=400, scaling=3
        )
      
      g <- ggplotGrob(pl)
      g$grobs[[which(g$layout$name == "panel-1-3")]] = nullGrob()
      g$grobs[[which(g$layout$name == "strip-t-3")]] = nullGrob()
      g$grobs[[which(g$layout$name == "axis-b-3")]] = nullGrob()
      
      "regression-to-mean-2" %>%
        save_plot({
          grid.draw(g)
        },
        width=1200, height=400, scaling=3
        )


      g$grobs[[which(g$layout$name == "panel-1-2")]] = nullGrob()
      g$grobs[[which(g$layout$name == "strip-t-2")]] = nullGrob()
      g$grobs[[which(g$layout$name == "axis-b-2")]] = nullGrob()
      "regression-to-mean-1" %>%
        save_plot({
          grid.draw(g)
        },
        width=1200, height=400, scaling=3
        )

})

set.seed(1)
v <- MASS::mvrnorm(200, c(0,0), matrix(c(1,0.5,0.5,1),2,2))<0.5
df <- data.frame(Crick=ifelse(runif(200)<0.5, "Yes","No"),
                Delta=ifelse(v[,1], "Yes","No"),
                Omicron=ifelse(v[,2], "Yes","No")
                )

local({
      df <- data.frame(
        Delta=c("Yes","No","No","Yes","No"))
      "one-categorical-a" %>%
        save_plot( {
          ggplot(df, aes(x=Delta, fill=Delta)) +
            geom_bar() +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("Yes","No"))) +
            theme_bw()
          },scaling=2)
      "one-categorical-b" %>%
        save_plot( {
          ggplot(df, aes(x="Delta", fill=Delta)) +
            geom_bar(position="fill") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("Yes","No"))) +
            labs(x="", y="Proportion") +
            theme_bw()
        }, scaling=2)
})

local({
  df <-expand.grid(Delta=c("+", "-"),
                  Location=c("Crick", "UCLH"))
  df$Count <- c(28, 12, 45, 14)
  df_prop <- group_by(df, Location) %>%
    mutate(Count=Count/sum(Count)) %>%
    ungroup()
  
    "two-categorical-a" %>%
        save_plot( {
          ggplot(df, aes(x=Location,y=Count, fill=Delta)) +
            geom_bar(stat="identity") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("+","-"))) +
            theme_bw()
          },scaling=3)
      "two-categorical-b" %>%
        save_plot( {
          ggplot(df_prop, aes(x=Location,y=Count, fill=Delta)) +
            geom_bar(stat="identity") +
            scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
            scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("+","-"))) +
            labs(y="Proportion") +
            theme_bw()
        }, scaling=3)
  ind <- rep(1:nrow(df), df$Count)
      "two-categorical-c" %>%
        save_plot( {
          ggplot(data=mutate(df[ind,], Delta=factor(Delta, levels=c("-","+")))) +
            geom_mosaic( aes(x=product(Location), fill=Delta)) +
            scale_fill_manual(values=setNames(FCICOL[c("red", "navy")], c("+","-"))) +
            theme_bw()
        }, scaling=3)
      "two-categorical-d" %>%
        save_plot( {
          ggplot(data=df[ind,]) + geom_mosaic( aes(x=product(Delta), fill=Location)) +
            scale_fill_manual(values=setNames(FCICOL[c("green", "orange")], c("Crick","UCLH"))) +
            theme_bw()
        }, scaling=3)

})
      

local({
    df <- data.frame(
      Treatment=c("Control","Control","Control", "M1","M1", "M2","M2"),
      Positive=c(5000,4567,6547, 3000,4567, 3230,1234),
      Total=c(9000,8000,9500, 9200,9994, 9300, 6000),
      Replicate=factor(c(1,2,3,1,2,1,2))
    )
  
    "cell-plate-scatter" %>%
      save_plot( {
        ggplot(df, aes(x=Treatment, y=Positive/Total, colour=Treatment)) +
          geom_point(size=3) +
          ylim(c(0,1)) +
          guides(colour="none") +
          theme_bw()
      }, scaling=3
      )
    
    "cell-plate-column" %>%
      save_plot( {
        ggplot(mutate(df, Negative=Total-Positive) %>% pivot_longer(c(Positive, Negative), names_to="State", values_to="Count"), aes(x=Replicate, y=Count, fill=State)) +
          geom_col() + facet_grid(~Treatment) +
          guides(fill="none") +
          labs(x=NULL) + 
          theme_bw()
      }, scaling=3
      )
})

local({
  set.seed(1)
  gx <- rep(seq(0,100, length=5), 40)
  gy <- rep(seq(80,20,length=5), 40)
  gp <- rep(LETTERS[1:5], 40)
  x <- rnorm(200,0,20)
  y <- rnorm(200,0,20)+x
  df <- data.frame(x=x+gx, y=y+gy)

  "simpsons0" %>%
  save_plot({
    ggplot(df, aes(x=x, y=y)) + geom_point() +
      labs(x="Treatment", y="Outcome") +
      theme_bw()
    }, scaling=3)

  "simpsons1" %>%
  save_plot({
    ggplot(df, aes(x=x, y=y)) + geom_point() +
      labs(x="Treatment", y="Outcome") +
      geom_smooth(method='lm', formula= y~x, se=FALSE) +
      theme_bw()
    }, scaling=3)

  "simpsons2" %>%
  save_plot({
  ggplot(df, aes(x=x, y=y, colour=gp)) + geom_point() +
    labs(x="Treatment", y="Outcome") +
    theme_bw() + guides(colour="none")
    }, scaling=3)

  "simpsons3" %>%
  save_plot({
  ggplot(df, aes(x=x, y=y, group=gp, colour=gp)) + geom_point() +
    labs(x="Treatment", y="Outcome") +
    geom_smooth(method='lm', formula= y~x, se=FALSE) +
    theme_bw() + guides(colour="none")
    }, scaling=3)

})
  
local({
  set.seed(2)
  N_sim <- 100
  N_rep <- 4
  mu1 <- 400
  mu2 <- 600
  sigma <- 120
  m1 <- matrix(rnorm(N_sim * N_rep, mu1, sigma), N_sim, N_rep)
  m2 <- matrix(rnorm(N_sim * N_rep, mu2, sigma), N_sim, N_rep)
  results <- data.frame(p=rep(NA_real_, N_sim),  b=I(as.list(1:N_sim)))
  results$m <- lapply(as.list(apply(cbind(m1, m2)[,order(c(1:N_rep, 1:N_rep))], 1, as.list)), unlist)
  for (i in 1:N_sim) {
    fit <- t.test(m1[i,], m2[i,])
    results$b[[i]] <- fit$estimate
    results[i, "p"] <- fit$p.value
  }
  L <- apply(results, 1, as.list)
  cat("var power_data=", jsonlite::toJSON(L, auto_unbox=TRUE), ";", file="d3-fig/power_data.js")
})  


local({
  
  set.seed(2)
  df <- data.frame(x=rnorm(200), y=rnorm(200)) %>%
    mutate(Hospital=ifelse(x+y>0.5 & runif(200)<0.7, "CVD","no"))


  "collider1" %>%
  save_plot({
  ggplot(df, aes(x=x, y=y)) + geom_point() +
    labs(x="Respiratory", y="Locomotor") +
    theme_bw() + guides(colour="none")
    }, scaling=3)

    "collider2" %>%
  save_plot({
  ggplot(df, aes(x=x, y=y)) + geom_point() +
    labs(x="Respiratory", y="Locomotor") +
          geom_smooth(method='lm', formula= y~x, se=FALSE) +
    theme_bw() + guides(colour="none")
    }, scaling=3)

  "collider3" %>%
  save_plot({
  ggplot(df, aes(x=x, y=y, colour=Hospital)) + geom_point() +
    labs(x="Respiratory", y="Locomotor") +
    annotate("text", x=-1.5, y=2, label="Hospitalised") +
    geom_smooth(method='lm', formula= y~x, se=FALSE) +
    theme_bw() + guides(colour="none")
    }, scaling=3)
})

local({
  set.seed(2)
  N_sim <- 100
  N_rep <- 4
  mu1 <- 350
  mu2 <- 650
  sigma <- 100
  m1 <- matrix(rnorm(N_sim * N_rep, mu1, sigma), N_sim, N_rep)
  m2 <- matrix(rnorm(N_sim * N_rep, mu2, sigma), N_sim, N_rep)
  results <- data.frame(p=rep(NA_real_, N_sim),  b=I(as.list(1:N_sim)))
  results$m <- lapply(as.list(apply(cbind(m1, m2)[,order(c(1:N_rep, 1:N_rep))], 1, as.list)), unlist)
  for (i in 1:N_sim) {
    fit <- t.test(m1[i,], m2[i,])
    results$b[[i]] <- fit$estimate
    results[i, "p"] <- fit$p.value
  }
  L <- apply(results, 1, as.list)
  cat("var power_data=", jsonlite::toJSON(L, auto_unbox=TRUE), ";", file="d3-fig/power_data2.js")
})  

local({
  set.seed(2)
  N_sim <- 100
  N_rep <- 7
  mu1 <- 400
  mu2 <- 600
  sigma <- 120
  m1 <- matrix(rnorm(N_sim * N_rep, mu1, sigma), N_sim, N_rep)
  m2 <- matrix(rnorm(N_sim * N_rep, mu2, sigma), N_sim, N_rep)
  results <- data.frame(p=rep(NA_real_, N_sim),  b=I(as.list(1:N_sim)))
  results$m <- lapply(as.list(apply(cbind(m1, m2)[,order(c(1:N_rep, 1:N_rep))], 1, as.list)), unlist)
  for (i in 1:N_sim) {
    fit <- t.test(m1[i,], m2[i,])
    results$b[[i]] <- fit$estimate
    results[i, "p"] <- fit$p.value
  }
  L <- apply(results, 1, as.list)
  cat("var power_data=", jsonlite::toJSON(L, auto_unbox=TRUE), ";", file="d3-fig/power_data3.js")
})  


local({
  set.seed(2)
  N_sim <- 100
  N_rep <- 4
  mu1 <- 400
  mu2 <- 600
  sigma <- 60
  m1 <- matrix(rnorm(N_sim * N_rep, mu1, sigma), N_sim, N_rep)
  m2 <- matrix(rnorm(N_sim * N_rep, mu2, sigma), N_sim, N_rep)
  results <- data.frame(p=rep(NA_real_, N_sim),  b=I(as.list(1:N_sim)))
  results$m <- lapply(as.list(apply(cbind(m1, m2)[,order(c(1:N_rep, 1:N_rep))], 1, as.list)), unlist)
  for (i in 1:N_sim) {
    fit <- t.test(m1[i,], m2[i,])
    results$b[[i]] <- fit$estimate
    results[i, "p"] <- fit$p.value
  }
  L <- apply(results, 1, as.list)
  cat("var power_data=", jsonlite::toJSON(L, auto_unbox=TRUE), ";", file="d3-fig/power_data4.js")
})  



if (FALSE) {par(mar = rep(0, 4))
plot(c(-0.15, 1.3), 0:1, type = "n", axes = FALSE)
text(0.6, 0.9, "all variables")
rect(0.4, 0.8, 0.8, 1)
text(0.25, 0.5, "numerical")
rect(0.1, 0.4, 0.4, 0.6)
arrows(0.45, 0.78, 0.34, 0.62, length = 0.08)
text(0.9, 0.5, "categorical")
rect(0.73, 0.4, 1.07, 0.6)
arrows(0.76, 0.78, 0.85, 0.62, length = 0.08)
text(0, 0.1, "discrete")
rect(-0.17, 0, 0.17, 0.2)
arrows(0.13, 0.38, 0.05, 0.22, length = 0.08)
text(0.39, 0.1, "continuous")
rect(0.25, 0, 0.53, 0.2)
arrows(0.35, 0.38, 0.4, 0.22, length = 0.08)
text(0.77, 0.105, "ordinal")
rect(0.64, 0, 0.9, 0.2)
arrows(0.82, 0.38, 0.77, 0.22, length = 0.08)
text(1.12, 0.1, "nominal")
rect(0.99, 0, 1.25, 0.2)
arrows(1.02, 0.38, 1.1, 0.22, length = 0.08)
}



local({
  
  x <- seq(-5,5,by=0.1)
  y <- seq(-5,5,by=0.1)
  xy <- as.matrix(expand.grid(x,y))
  z <- matrix(dmvnorm(xy, mean=c(0,0), sigma=matrix(c(1,.8,.8,3),2,2)), length(x), length(y))
  fig <- plot_ly(x = ~x, y=~y, z = ~z) %>%
    add_surface() %>%
    layout(scene = list(xaxis=list(title="EG1"), yaxis=list(title="EG2"), zaxis=list(title="Density"),
                        camera = list(projection = list(type = 'orthographic'))
                        )
           ) %>%
    hide_colorbar()
  
  saveWidget(fig, "p1.html", selfcontained = F, libdir = "plotly")
  file.rename("p1.html", "d3-fig/p1.html")

  z <- 0.5 * matrix(dmvnorm(xy,mean=c(3, -3), sigma=matrix(c(1,.7,.7,1),2,2)), length(x), length(y)) +
    0.5 * matrix(dmvnorm(xy, mean=c(-3,3), sigma=matrix(c(1,.7,.7,1),2,2)), length(x), length(y))
  fig <- plot_ly(x = ~x, y=~y, z = ~z)
  fig <- fig %>%
    add_surface() %>%
    layout(
      scene = list(
        xaxis=list(title="EG1"), yaxis=list(title="EG2"), zaxis=list(title="Density"),
        camera = list(
          projection = list(type = 'orthographic')
        )
      )
    ) %>%
    hide_colorbar()
  
  saveWidget(fig, "p2.html", selfcontained = F, libdir = "plotly")
  file.rename("p2.html", "d3-fig/p2.html")
  
  samp <- data.frame(rbind(
    rmvnorm(100, mean=c(3, -3), sigma=matrix(c(1,.7,.7,1),2,2)),
    rmvnorm(100, mean=c(-3,3), sigma=matrix(c(1,.7,.7,1),2,2))
  ))
  names(samp) <- c("EG2","EG1")
  pc <- prcomp(samp)
  v <- pc$rotation
  sl <- v[2,1]/v[1,1]
  pl <-   ggplot(samp, aes(x=EG1, y=EG2)) +
    geom_point(size=2) +
    coord_fixed() +
    theme_bw()

  "PCA1" %>%
    save_plot({
      pl
    }, scaling=2)
  
  "PCA2" %>%
    save_plot({
      pl+    geom_abline(intercept=pc$center["EG2"], slope=sl, size=1)
    }, scaling=2)
  
  "PCA3" %>%
    save_plot({
      pl+    geom_abline(intercept=pc$center["EG2"], slope=-1/sl, size=1)
    }, scaling=2)
  
  pc <- prcomp(samp[(1:100),])
  v <- pc$rotation
  sl <- v[2,1]/v[1,1]
  pl <-   ggplot(samp, aes(x=EG1, y=EG2)) +
    geom_point(size=2, colour=rep(c("grey","black"), each=100)) +
    coord_fixed() +
    theme_bw()
  "PCA4" %>%
    save_plot({
      pl+    geom_abline(intercept=-6, slope=sl, size=1)
    }, scaling=2)
  

})

      
local({
  
    set.seed(2)
    df <- expand.grid(ID=1:2, sample_label=1, treatment=factor(c("Control","Treated")), time=factor(c(0,4,12,24)))
    df <- subset(df, treatment!="Treated" | time!=0)
    df$ct <- as.integer(as.character(df$time))
    df$sample_label <- paste(df$treatment, df$time, df$ID, sep="_")
    df$ID <- df$sample_label
    write.table(df, file="extdata/experiment_table.csv", sep=",", quote=FALSE, row.names=FALSE)
    set.seed(2);df$Response <- rnorm(nrow(df), 2, .1)

    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    df$Response <- df$Response + 2-df$pred
    df$pred <- predict(lm(Response ~ time:treatment, data=df))

    df_orig <- df
    
    "cts1" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_line(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") +
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )

    df <- mutate(df,
                Response=Response + ifelse(treatment=="Treated", 2,0)
                )
    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    "cts2" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_line(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )

    df_treat <- df
    
    df <- mutate(df,
                Response=Response + c(0,0.4,-0.5,-0.5)[match(time, levels(time))]
                )
    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    "cts3" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_path(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )

    df <- mutate(df,
                Response=Response + ifelse(treatment=="Treated" & ct==24, -1.5,0)
                )
    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    "cts4" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_path(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )

    df <-  mutate(df_treat,
                Response=Response + 0.02*ct
                )
    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    "cts5" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_path(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )
    
    df <-  mutate(df_orig,
                Response=Response + ifelse(treatment=="Treated", 0.1*ct, 0.05*ct)
                )
    df$pred <- predict(lm(Response ~ time:treatment, data=df))
    "cts6" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_path(aes(y=pred)) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )

    df <-  mutate(df_treat,
                Response=Response + ((ct-10)^2)/100 - 1.5
                )
    new_data <- expand.grid(treatment=unique(df$treatment), ct=seq(0,24,.5))
    new_data$pred <- predict(lm(Response ~ poly(ct,2)*treatment,data=df), newdata=new_data)
    "cts7" %>%
      save_plot( {
        ggplot(df, aes(x=ct, y=Response, colour=treatment, group=treatment)) +
          geom_point(size=3) +
          geom_path(aes(y=pred), data=new_data) + 
          scale_colour_manual(values=setNames(FCICOL[c("red", "navy")], c("Treated","Control"))) +
          ylim(0,5) + labs(x="Time") + 
          theme_bw() +
          theme(legend.position="bottom")
      },
      width=800, height=800, scaling=3
      )
})


"corr_high" %>%
  save_plot( {
    set.seed(1)
    df <- as.data.frame(rmvnorm(100, mean = c(0,1), sigma = matrix(c(1,.8,.8,2), 2, 2)))
    names(df) <- c("EG1","EG2")
    ggplot(df, aes(x=EG1, y=EG2)) +
      geom_point(size=3) +
      theme_bw()
  },
  width=800, height=800, scaling=3
  )

"corr_medium" %>%
  save_plot( {
    set.seed(1)
    df <- as.data.frame(rmvnorm(100, mean = c(0,1), sigma = matrix(c(1,.5,.5,2), 2, 2)))
    names(df) <- c("EG1","EG2")
    ggplot(df, aes(x=EG1, y=EG2)) +
      geom_point(size=3) +
      theme_bw()
  },
  width=800, height=800, scaling=3
  )

"corr_none" %>%
  save_plot( {
    set.seed(1)
    df <- as.data.frame(rmvnorm(100, mean = c(0,1), sigma = matrix(c(1,0,0,2), 2, 2)))
    names(df) <- c("EG1","EG2")
    ggplot(df, aes(x=EG1, y=EG2)) +
      geom_point(size=3) +
      theme_bw()
  },
  width=800, height=800, scaling=3
  )

"corr_bi" %>%
  save_plot( {
    set.seed(1)
    df <- as.data.frame(
      rbind(
        rmvnorm(50, mean = c(0,1), sigma = matrix(c(1,0,0,2), 2, 2)),
        rmvnorm(50, mean = c(5,-4), sigma = matrix(c(1,0,0,2), 2, 2))
      )
    )
    names(df) <- c("EG1","EG2")
    ggplot(df, aes(x=EG1, y=EG2)) +
      geom_point(size=3) +
      theme_bw()
  },
  width=800, height=800, scaling=3
  )



  
     
