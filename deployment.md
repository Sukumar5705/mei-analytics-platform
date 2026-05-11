# Deployment Guide

## Local Deployment

Run:

```r
shiny::runApp()
```

---

# Deploy on shinyapps.io

## Install rsconnect

```r
install.packages("rsconnect")
```

---

## Authenticate

```r
library(rsconnect)

rsconnect::setAccountInfo(
  name='YOUR_NAME',
  token='TOKEN',
  secret='SECRET'
)
```

---

## Deploy Application

```r
rsconnect::deployApp()
```

---

# Recommended Deployment Platforms

- shinyapps.io
- Posit Connect
- Docker + Shiny Server
- AWS EC2
