library(shiny)

# Stable vars to use when adding one sample at a time
means <- NULL
means_offset <- 0

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    rheavy <- function(n) return(rt(n,6))
    rheavyyy <- function(n) return(rt(n,2.25))
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   heavy = rheavy,
                   heavyyy = rheavyyy,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    # in addition to input$dist and input$n react to changes in...
    input$resample
    input$checkbox
    input$reps
    input$tstat
    
    dist(input$n) # draw n samples
  })
  
  # Reactive expression to update the list of means when the distribution changes
  doReset <- reactive({
    rheavy <- function(n) return(rt(n,6))
    rheavyyy <- function(n) return(rt(n,2.25))
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   heavy = rheavy,
                   heavyyy = rheavyyy,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    # in addition to input$dist react to changes in...
    input$checkbox
    input$n
    input$reps
    input$tstat
    
    means<<-NULL
    tstats<<-NULL
    print("reset")
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    # Plot parameters...
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=2;          # label rescaling factor
    
    dist <- input$dist
    n <- input$n
    reps <- input$reps
    tstat <- input$tstat
    x<-data()
    doReset()
    
    mu = switch(dist,
                norm = 0,
                unif = 0.5,
                heavy = 0,
                heavyyy = 0,
                lnorm = exp(1/2),
                exp = 1,
                0)
    sigma = switch(dist,
                   norm = 1,
                   unif = 1/sqrt(12),
                   heavy = sqrt(3/2),
                   heavyyy = 3,
                   lnorm = sqrt((exp(1)-1)*(exp(1))),
                   exp = 1,
                   0)
    xbar <<- mean(x)
    s <<- sd(x)
    tstat.tmp <<- switch(tstat,
                         z = (xbar-mu)/(sigma/sqrt(n)),
                         t = (xbar-mu)/(s/sqrt(n)),
                         chisq = (n-1)*s^2/sigma^2,
                         0)
  
    # Add to list of sample means or repeat sampling reps times depending on checkbox
    if (input$checkbox) {
      if (length(means)==0) {means_offset<<-input$resample}
      means[input$resample-means_offset+1]<<-xbar
      tstats[input$resample-means_offset+1]<<-tstat.tmp
    }
    else {
      means<<-1:reps
      tstats<<-1:reps
      for (i in 1:reps) {
        rheavy <- function(n) return(rt(n,6))
        rheavyyy <- function(n) return(rt(n,2.25))
        tmpdata <<- switch(dist,
                           norm = rnorm(n),
                           unif = runif(n),
                           heavy = rheavy(n),
                           heavyyy = rheavyyy(n),
                           lnorm = rlnorm(n),
                           exp = rexp(n),
                           rnorm(n))
        xbar <<- mean(tmpdata)
        s <<- sd(tmpdata)
        tstat.tmp2 <<- switch(tstat,
                             z = (xbar-mu)/(sigma/sqrt(n)),
                             t = (xbar-mu)/(s/sqrt(n)),
                             chisq = (n-1)*s^2/sigma^2,
                             0)
        means[i] <<-xbar
        tstats[i] <<- tstat.tmp2
      }
    }
    
    # set plot range
    xmin = switch(dist, norm = -3, unif = 0, heavy = -4.5, heavyyy = -6, lnorm = 0, exp = 0, -3)
    xmax = switch(dist, norm =  3, unif = 1, heavy =  4.5, heavyyy =  6, lnorm = 4, exp = 4,  3)
    tsmin = switch(tstat, z = -3, t =  qt(.001,n-1), chisq = 0)
    tsmax = switch(tstat, z =  3, t =  qt(.999,n-1), qchisq(.9999,n-1))
    
    # do not plot outliers
    xrm<-x
    xrm[x>xmax]<-NA
    xrm[x<xmin]<-NA
    means[means>xmax]<<-NA
    means[means<xmin]<<-NA
    print("before trm")
    trm<-tstats
    print(length(trm))
    print(str(trm))
    trm[tstats<tsmin]<-NA
    trm[tstats>tsmax]<-NA
    print("removed outliers")
    
    par(mfrow=c(3,1),mar=c(8,6,2,2)) 
    
    # plot true distribution
    x0 = seq(xmin,xmax,length.out=512);
    dheavy <- function(x) return(dt(x,6))
    dheavyyy <- function(x) return(dt(x,2.25))
    y0 = switch(dist,
                norm = dnorm(x0),
                unif = dunif(x0),
                heavy = dheavy(x0),
                heavyyy = dheavyyy(x0),
                lnorm = dlnorm(x0),
                exp = dexp(x0),
                dnorm(x0))
    #y0=y0/sum(y0);
    plot(x0,y0,type="l",lwd=0,col=NULL,main="Population",xlab="",ylab="Density",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale) 
    polygon(c(xmin,x0,xmax),c(0,y0,0),col=tcol,border=NA)
    
    
    # plot typical sample
    hist(xrm, 
         breaks=seq(xmin,xmax,length.out=50),
         main="Typical Sample",
         warn.unused = FALSE,
         col=tcol,
         border=tcol,
         xlab="",
         cex.lab=tscale,
         cex.axis=tscale,
         cex.main=tscale,
         cex.sub=tscale)
    if (any(x<xmin)) {
      points(rep(xmin-0.1,sum(x<xmin)),rbeta(sum(x<xmin),2,2),lwd=2,col=tcol,cex=tscale)
    }
    if (any(x>xmax)) {
      points(rep(xmax+0.1,sum(x>xmax)),rbeta(sum(x>xmax),2,2),lwd=2,col=tcol,cex=tscale)
    }
    
  # plot list of test statistics with the latest sample highlighted and N(mu,sigma^2/n)
  breaks_mh=seq(tsmin,tsmax,length.out=100);
  t0 = seq(tsmin,tsmax,length.out=512);
  y0 = switch(tstat,
              z = dnorm(t0,0,1),
              t = dt(t0,n-1),
              chisq = dchisq(t0,n-1),
              0)
  y0=y0/sum(y0)*length(tstats)*mean(diff(breaks_mh))/mean(diff(t0))
  
  nh<-hist(trm,
           breaks=breaks_mh,
           main="Test Statistics",
           warn.unused = FALSE,
           col=tcol,
           border=tcol,
           xlab="",
           cex.lab=tscale,
           cex.axis=tscale,
           cex.main=tscale,
           cex.sub=tscale,
           plot=FALSE)
  hist(trm,
       breaks=breaks_mh,
       main="Test Statistics",
       warn.unused = FALSE,
       col=tcol,
       border=tcol,
       xlab="",
       cex.lab=tscale,
       cex.axis=tscale,
       cex.main=tscale,
       cex.sub=tscale,
       plot=TRUE,
       ylim=c(0,max(y0,max(nh$counts))))
  if (tstat.tmp>tsmin && tstat.tmp<tsmax) {
    hist(mean(tstat.tmp),
         breaks=breaks_mh,
         col=acol,
         border=acol,
         add=TRUE,
         ylim=c(0,max(y0,max(nh$counts))))
  }
  points(t0,y0,type="l",lwd=2)
  print(input$resample)
},width=600,height=600)
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
})