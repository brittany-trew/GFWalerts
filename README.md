# GFWalerts

`GFWalerts` is an R package that interacts with the Global Forest Watch (GFW) API to request and download integrated deforestation alerts. It allows users to easily download the integrated deforestation alerts from Global Forest Watch using R.

## Installation

You can install the development version from GitHub with:

```r
# Install devtools if necessary
install.packages("devtools")

# Install the GFWalerts package
devtools::install_github("brittany-trew/GFWalerts")
```


## Usage

Here is a basic example of how to use the package to retrieve deforestation alerts:

```r
library(GFWalerts)

# Request an authentication token.
auth_token <- request.token(username = "your_username", pw = "your_password")

# Define geographical extent.
geom_ext <- c(xmin = -11.51, xmax = -9.99, ymin = 7.1, ymax = 8.15)

# Define the target month.
current_date <- Sys.Date()
months_back <- 1
target_date <- find.target.MY(current_date, months_back)

# Download GFW alerts for a specific date range.
alerts <- run.download.month(target_date, api.key, geom.ext = target.ext)


# View the alerts
print(alerts)
```

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
