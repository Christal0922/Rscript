
#计算统计量----------------
#' @title 非参组间比较
#'
#' @description
#' \code{oneway}计算非参组间比较，包括综合检验和事后成对组间比较
#'
#' @details
#' 这个函数计算了一个综合Kruskal-Wallis检验，用于检验组别是否相等，接着使用Wilcoxon秩和检验来
#' 进行成对比较。如果因变量之间没有相互依赖的话，可以计算精确的Wilcoxon检验。使用\code{\link{p.
#' adjust}}来对多重比较所得到的p值进行调整
#'
#' @param formula一个formula类的对象，用于表示因变量和分组变量之间的联系
#' @param data一个包含了模型里变量的数据框
#' @param exact逻辑型变量。如 \code{TRUE}，计算精确的Wilcoxon检验
#' @param sort逻辑型变量。如果\code{TRUE}，用因变量中位数来对组别进行排序
#' @param用于调整多重比较的p值的方法
#' @export
#' @return 一个有7个元素的列表：
#' \item{CALL}{函数调用}
#' \item{data}{包含因变量和组间变量的数据库}
#' \item{sumstates}{包含每组的描述性统计量的数据库}
#' \item{kw}{Kruskal-Wallis检验的结果}
#' \item{method}{用于调整p值的方法}
#' \item{wmc}{包含多重比较的数据框}
#' \item{vnames}{变量名}
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' summary(results)
#' plot(results, col="lightblue", main="Multiple Comparisons",
#' xlab="US Region", ylab="Healthy Life Expectancy at Age 65")
oneway <- function(formula, data, exact=FALSE, sort=TRUE,
                   method=c("holm", "hochberg", "hommel", "bonferroni",
                            "BH", "BY", "fdr", "none")){
  if (missing(formula) || class(formula) != "formula" ||
      length(all.vars(formula)) != 2)
    stop("'formula' is missing or incorrect")

  method <- match.arg(method)
  df <- model.frame(formula, data)
  y <- df[[1]]
  g <- as.factor(df[[2]])
  vnames <- names(df)
  if(sort) g <- reorder(g, y, FUN=median)
  groups <- levels(g)
  k <- nlevels(g)
  getstats <- function(x)(c(N = length(x), Median = median(x),
                            MAD = mad(x)))
  sumstats <- t(aggregate(y, by=list(g), FUN=getstats)[2])
  rownames(sumstats) <- c("n", "median", "mad")
  colnames(sumstats) <- groups
  kw <- kruskal.test(formula, data)
  wmc <- NULL
  for (i in 1:(k-1)){
    for (j in (i+1):k){
      y1 <- y[g==groups[i]]
      y2 <- y[g==groups[j]]
      test <- wilcox.test(y1, y2, exact=TRUE)
      r <- data.frame(Group.1=groups[i], Group.2=groups[j],
                      W=test$statistic[[1]], p=test$p.value)
      # note the [[]] to return a single number
      wmc <- rbind(wmc, r)
    }
  }
  wmc$p <- p.adjust(wmc$p, method=method)
  data <- data.frame(y, g)
  names(data) <- vnames
  results <- list(CALL = match.call(),
                  data=data,
                  sumstats=sumstats, kw=kw,
                  method=method, wmc=wmc, vnames=vnames)
  class(results) <- c("oneway", "list")
  return(results)
}

#打印结果-----------------
#' @title打印多重比较的结果
#'
#' @description
#' \code{print.oneway}打印了多重组间比较的结果
#'
#' @details
#' 这个函数打印出用\code{\link{oneway}}函数所创建的Wilcoxon成对多重比较的结果
#'
#' @param x一个\code{oneway}类型的变量
#' @param ... 要传输给函数的额外的变量
#' @method print oneway
#' @export
#' @return静默返回输入的对象
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' print(results)
print.oneway <- function(x, ...){
  if (!inherits(x, "oneway"))
    stop("Object must be of class 'oneway'")
  cat("data:", x$vnames[1], "by", x$vnames[2], "\n\n")
  cat("Multiple Comparisons (Wilcoxon Rank Sum Tests)\n")
  cat(paste("Probability Adjustment = ", x$method, "\n", sep=""))
  print(x$wmc, ...)
}

#汇总结果----------------
#' @title汇总单因子非参分析的结果
#'
#' @description
#' \code{summary.oneway}汇总了单因子非参分析的结果
#' nonparametric analysis.
#'
#' @details
#' 这个函数对\code{\link{oneway}}函数所分析的结果进行汇总并打印。这包括了每一组的描述性统计量，
#' 一个综合Kruskal-Wallis检验的结果，以及一个Wilcoxon成对多重比较的结果
#'
#' @param object一个\code{oneway}类型的对象
#' @param ... 额外的参数
#' @method summary oneway
#' @export
#' @return静默返回输入的对象
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' summary(results)
summary.oneway <- function(object, ...){
  if (!inherits(object, "oneway"))
    stop("Object must be of class 'oneway'")
  if(!exists("digits")) digits <- 4L
  kw <- object$kw
  wmc <- object$wmc
  cat("data:", object$vnames[1], "on", object$vnames[2], "\n\n")
  cat("Omnibus Test\n")
  cat(paste("Kruskal-Wallis chi-squared = ",
            round(kw$statistic,4),
            ", df = ", round(kw$parameter, 3),
            ", p-value = ",
            format.pval(kw$p.value, digits = digits),
            "\n\n", sep=""))
  cat("Descriptive Statistics\n")
  print(object$sumstats, ...)
  wmc$stars <- " "
  wmc$stars[wmc$p < .1] <- "."
  wmc$stars[wmc$p < .05] <- "*"
  wmc$stars[wmc$p < .01] <- "**"
  wmc$stars[wmc$p < .001] <- "***"
  names(wmc)[which(names(wmc)=="stars")] <- " "
  cat("\nMultiple Comparisons (Wilcoxon Rank Sum Tests)\n")
  cat(paste("Probability Adjustment = ", object$method, "\n", sep=""))
  print(wmc, ...)
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' '
1\n")
}

#绘制结果----------
#' @title对非参组间比较的结果进行绘图
#'
#' @description
#' \code{plot.oneway}对非参组间比较的结果进行绘图
#'
#' @details
#' 这个函数使用标记了的并排箱线图对\code{\link{oneway}}函数所生成的非参组间比较结果进行绘图。
#' 中位数和样本量被放置在图的上方。总体中位数用一条虚横线进行表示
#'
#' @param x一个\code{oneway}类型的对象
#' @param ... 被传递给\code{\link{boxplot}}函数的额外参数
#' @method plot oneway
#' @export
#' @return NULL
#' @author Rob Kabacoff <rkabacoff@@statmethods.net>
#' @examples
#' results <- oneway(hlef ~ region, life)
#' plot(results, col="lightblue", main="Multiple Comparisons",
#' xlab="US Region", ylab="Healthy Life Expectancy at Age 65")
plot.oneway <- function(x, ...){
  if (!inherits(x, "oneway"))
    stop("Object must be of class 'oneway'")
  data <- x$data
  y <- data[,1]
  g <- data[,2]
  stats <- x$sumstats
  lbl <- paste("md=", stats[2,], "\nn=", stats[1,], sep="")
  opar <- par(no.readonly=TRUE)
  par(mar=c(5,4,8,2))
  boxplot(y~g, ...)
  abline(h=median(y), lty=2, col="darkgrey")
  axis(3, at=1:length(lbl), labels=lbl, cex.axis=.9)
  par(opar)
}

#添加样本数据到包-------------
region <- c(rep("North Central", 12), rep("Northeast", 9),
            rep("South", 16), rep("West", 13))
state <- c("IL","IN","IA","KS","MI","MN","MO","NE","ND","OH","SD","WI",
           "CT","ME","MA","NH","NJ","NY","PA","RI","VT","AL","AR","DE",
           "FL","GA","KY","LA","MD","MS","NC","OK","SC","TN","TX","VA",
           "WV","AK","AZ","CA","CO","HI","ID","MT","NV","NM","OR","UT",
           "WA","WY")
hlem <- c(12.6,12.2,13.4,13.1,12.8,14.3,11.7,13.1,12.9,12.2,13.3,13.4,
          14.3,13.5,13.8,14,12.9,13.6,12.8,13.1,13.9,10.3,11.6,13.5,
          14.3,11.6,10.2,11.6,13.3,10.1,11.7,10.8,12,11.2,12.2,13.3,
          10.3,13.3,13.7,13.8,14.3,15,13.1,13.4,12.8,13.1,13.9,14.3,14,
          13.7)
hlef <- c(14.3,14.1,15.9,15.1,14.7,16.7,14,15.7,16,14,16.4,16.1,16.7,
          15.7,15.9,16,14.8,15.3,14.8,15.6,16.2,11.7,12.7,15.7,16.4,
          13.1,11.6,12.3,15.3,11.4,13.5,12.9,13.6,12.5,13.4,14.9,11.6,
          14.9,16.3,15.5,16.2,17.3,15.1,15.6,14.5,14.7,16,15.7,16,15.2)
life <- data.frame(region=factor(region), state=factor(state), hlem, hlef)
save(life, file='life.rda')
#' @title 65岁时的健康预期寿命
#'
#' @description表示65岁时健康预期寿命（预期在良好健康状况下持续多少年）的数据集，基于美国不同州
#' 在2007到2009年的数据。男性女性的预期值分开记录
#'
#' @docType data
#' @keywords datasets
#' @name life
#' @usage life
#' @format一个包含了50行和4个变量的数据框。变量分别为
#' \describe{
#' \item{region}{一个有4个类别的因子型变量（North Central（中北部）、Northeast（东北部）、
#' South（南部）、West（西部））}
#' \item{state}{一个包含了美国50个州的因子型变量，每个州用ISO标准的2个字母进行表示}
#' \item{hlem}{用年份表示的男性健康预期寿命}
#' \item{hlef}{用年份表示的女性健康预期寿命}
#' }
#' @source数据\code{hlem}和\code{hlef}从疾病预防和控制中心\emph{Morbidity and Mortality
#' Weekly Report}的\url{http://www.cdc.gov/mmwr/preview/mmwrhtml/mm6228a1.htm?s_
#' cid= mm6228a1_w}网站上获取
#' 变量\code{region}从\code{\link[datasets]{state.region}}数据集中所获取
NULL

# Description -----------
# Package: npar
# Type: Package
# Title: Nonparametric group comparisons
# Version: 1.0
# Date: 2015-01-26
# Author: Rob Kabacoff
# Maintainer: Robert Kabacoff <robk@statmethods.net>
#   Description: This package assesses group differences using nonparametric
# statistics. Currently a one-way layout is supported. Kruskal-Wallis
# test followed by pairwise Wilcoxon tests are provided. p-values are
# adjusted for multiple comparisons using the p.adjust() function.
# Results are plotted via annotated boxplots.
# LazyData: yes
# License: GPL-3









