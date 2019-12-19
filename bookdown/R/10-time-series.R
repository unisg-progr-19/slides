#' # Time Series
#'
#' [Intro Slides](10-time-series.html)
#'
#' We will cover the basic handling of time series in R. We will meet quarterly
#' and monthly time series as well as daily time series, and check out ways to
#' forecast them. We will also cover the tsbox package, which allows you to
#' convert between various time series classes.
#'
#' ## Time Series Objects
#'
#' R offers a build-in object class that facilitates working with time series
#' data. Like matrices, time series objects (or `ts` objects) are basically a
#' vector with an additional attribute. In order to create a `ts` object, use
#' the `ts` function:

simple_ts <- ts(c(4, 2, 1, 2, 3, 4, 4, 3, 5), start = c(2015, 4), frequency = 4)
class(simple_ts)

#' We will cover the *tsbox* package in a second. Among other, it gives you a
#' convenient way to plot all kind of time series:

library(tsbox)
ts_plot(simple_ts)

#' The first argument is the data, a vector.
#'
#' The `start` argument describes the beginning of the time series. It is
#' usually a vector of length 2; first the year, then the higher frequency
#' period -- the quarter, the month, the day.
#'
#' The `frequency` argument describes the frequency. It is 4 in our case,
#' indicating quarterly data, but may be any other integer value.
#'
#' There are many build-in example time series R. A popular is `AirPassengers`,
#' which shows monthly totals of international airline passengers, from 1949 to
#' 1960.

AirPassengers
ts_plot(AirPassengers)

#' ## Economic Time Series
#'
#' More often than constructing your own time series, you will get time series
#' from other sources, e.g., from spreadsheets, from databases, or from the
#' Internet.
#'
#' There is a vast number of on-line data sources for time series, as well as a
#' plethora of R packages to access them. For economic data, the [FRED
#' database](https://fred.stlouisfed.org) is one of the best free on-line data
#' source. However, it needs (free) registration and an API key, so we will use
#' a different package in the following. A partial list with packages can be
#' found [here](https://cengel.github.io/gearup2016/SULdataAccess.html().
#' Another good website for economic data is
#' [dbnomics](https://db.nomics.world).

# install.packages("rdbnomics")
library(tidyverse)
library(rdbnomics)

swiss_gdp <-
  rdb(ids = c(
    "Eurostat/namq_10_gdp/Q.CLV05_MEUR.NSA.B1G.CH"
  )) %>%
  select(period, value) %>%
  ts_ts()

ts_plot(swiss_gdp)

#' ## Financial Time Series
#'
#' For financial data, a popular package is quantmod, which allows you to access
#' multiple data sources.
#'
#' Let's get Apple stock data; Apple's ticker symbol is `AAPL`. We use the
#' quantmod function `getSymbols`, and pass a string as a first argument to
#' identify the desired ticker symbol, pass `'yahoo'` to `src` for Yahoo!
#' Finance. Always use `auto.assign = TRUE` in `getSymbols()`. The default
#' behavior is confusing and should be avoided.

library(quantmod)
apple <-
  getSymbols("AAPL", src = "yahoo", auto.assign = FALSE) %>%
  ts_tbl() %>%
  filter(id == "AAPL.Close")

#' `tsbox::ts_tbl` converts the output of `getSymbols` to a tibble data frame,
#' which is much more useful. We will cover it later on. We use filter to focus
#' on closing prices.

ts_plot(apple)

#' This data covers business days only, so it is not regular. `ts` objects are
#' not suitable to store data like this. Usually, data frames are a good
#' alternative, but there are many other time series classes that deal with
#' daily time series in R. We will meet some of them later on.
#'
#' ## Automatic ARIMA Forecasting with the 'forecast' package
#'
#' The `forecast` package offers many useful functions for working with regular
#' time-series, such as `swiss_gdp`. We will just cover a single function that
#' allows you to automatically build an ARIMA model.
#'
#' For more information on ARIMA based time series forecasting in R, please
#' refer to the free online book, *Forecasting: principles and practice*, by Rob
#' Hyndman:
#'
#' [https://otexts.com/fpp2/](https://otexts.com/fpp2/)
#'

library(forecast)

#' If you don't know much about a time series, but want to produce a simple
#' forecast without external indicators, the `auto.arima` function is your
#' friend. Let's say we want to know what will happen to `AirPassengers` in
#' 1961.

m_arima <- auto.arima(AirPassengers)
m_arima
broom::tidy(m_arima)

#' As the 'auto' indicates, the function does everything on its own. First,
#' it made the series stationary, by taking first differences and seasonal
#' differences. Second, on the stationary series, it evaluates what it finds
#' to be the best ARMA model. AR and MA models are just special cases of ARMA
#' models.
#'
#' With these details you could also have fed the built-in `arima` function, to
#' verify the result of the estimation:

arima(AirPassengers, order = c(2, 1, 1), seasonal = c(0, 1, 0))

#' In order to produce a forecast, the `forecast` package has a function
#' `forecast()`, which can be applied on the output of `auto.arima()`. The `h`
#' argument indicates the number of periods to be forecasted:

fct_arima <- forecast(m_arima, h = 12)
plot(fct_arima)

#' In order to access the point forecast, you can access the `mean` component of
#' the `forecast()` output:

fct_arima$mean

#' ## Seasonal Adjustment
#'
#' Many time series exhibit a regular seasonal pattern over the year. US
#' unemployment, for example, is usually higher from January to March, and again
#' in June and July. Similarly, retail sales tend to peak with the Christmas
#' season.
#'
#' To model the underlying structure of these series, any regular (seasonal)
#' patterns are estimated and removed from the data. For example, to see if the
#' economy is moving out of a recession during certain months, one wants the
#' labor market data to be free from such seasonal effects. Seasonal adjustment
#' decomposes a time series into a trend, a seasonal and an irregular component
#' and removes the seasonal component from the data.
#'
#' The seasonal packages gives access to X-13ARIMA-SEATS, the seasonal
#' adjustment software from the U.S. Census bureau. It is the standard software
#' for seasonal adjustment of regular time series and takes care of seasonal
#' effects, Easter effects and trading day adjustment.
#'
#' Basic usage is very simple and offers a good seasonal adjustment in most
#' cases:

library(seasonal)
m_seas <- seas(AirPassengers)
plot(m_seas)
ts_plot(final(m_seas))


#' ## Forecasting daily time series
#'
#' While `auto.arima()` and `seas()` work well with regular data, they are not
#' suitable for daily data. An easy to use forecasting tool for daily (or even
#' hourly) data is Facebook's R package prophet. Note that such a forecast is
#' purely based on historical information, and may have close to zero value for
#' for financial data. Let's try it nevertheless.


# install.packages("prophet")
library(prophet)

#' We call the prophet function to fit the model. The first argument is the
#' historical data frame. It must have columns `ds`, for the time stamp, and `y`,
#' the value of the series.

df <- apple %>%
  select(ds = time, y = value)
m_prophet <- prophet(df)

#' prophet checks for daily, weekly, and yearly seasonality, as well as for
#' holiday effects. Because our data is daily, there is no daily seasonality.
#' This would only appear, e.g., in hourly data.
#'
#' Predictions are made on a data frame with a column ds containing the dates
#' for which predictions are to be made. `make_future_dataframe()` takes the
#' model object and a number of periods to forecast and produces a suitable data
#' frame.

df_future <-
  make_future_dataframe(m_prophet, periods = 365)
tail(df_future)

#' As with most modeling procedures in R, we use the generic predict function to
#' get our forecast. The forecast object is a data frame with a column `yhat`
#' containing the forecast. It has additional columns for uncertainty intervals
#' and seasonal components.

fct_prophet <-
  predict(m_prophet, df_future) %>%
  as_tibble()

fct_prophet %>%
  select(ds, yhat) %>%
  tail()

#' You can use the generic plot function to plot the forecast, by passing in the
#' model and the forecast data frame.

plot(m_prophet, fct_prophet)

#' You can use `prophet_plot_components() to see the forecast broken down into
#' trend, weekly seasonality, and yearly seasonality:

prophet_plot_components(m_prophet, fct_prophet)

#' ## Exercises
#'
#' 1. Estimate an automated ARIMA model of Swiss GDP.
#'
#' 2. Inspect the model: What is the degree of seasonal and non-seasonal
#' differentiation, what are the seasonal and non-seasonal AR and MA orders?
#'
#' 3. Re-estimate 1, using R base `arima()` function
#'
#' 4. Seasonally adjust Swiss GDP. Compare the ARIMA model with the one from
#' `auto.arima`.
#'
#' 5. Download your stock title of choice. Use prophet to forecast the series.
#'
#'
#' ## From `ts` to `tibble`: The tsbox package
#'
#' The R ecosystem knows a vast number of time series classes: ts, xts, zoo,
#' tsibble, tibbletime or timeSeries. The plethora of standards causes
#' confusion. As different packages rely on different classes, it is hard to use
#' them in the same analysis. tsbox provides a set of tools that make it easy to
#' switch between these classes. It also allows the user to treat time series as
#' plain data frames, facilitating the use with tools that assume rectangular
#' data. It has it's own [website](https://www.tsbox.help/).
#'
#' tsbox is built around a set of converters, which convert time series stored
#' as **ts**, **xts**, **data.frame**, **data.table**, **tibble**, **zoo**,
#' **tsibble**, **tibbletime** or **timeSeries** to each other.
#'
#' Because this works reliably, we can easily write functions that work for all
#' classes. So whether we want to smooth, scale, differentiate, chain, forecast,
#' regularize or seasonally adjust a time series, we can use the same commands
#' to whatever time series class at hand. And, most conveniently, we get a time
#' series plot function that works for all classes and frequencies.
#'
#' ### Convert everything to everything
#'
#' tsbox can convert time series stored as **ts**, **xts**, **data.frame**,
#' **data.table**, **tibble**, **zoo**, **tsibble**,  **tibbletime** or
#' **timeSeries** to each other:

library(tsbox)
x_ts <- ts_c(fdeaths, mdeaths)
x_xts <- ts_xts(x_ts)
x_df <- ts_df(x_xts)
x_dt <- ts_df(x_df)
x_tbl <- ts_tbl(x_dt)

# and more exotic stuff
x_zoo <- ts_zoo(x_tbl)
x_tsibble <- ts_tsibble(x_zoo)
all.equal(ts_ts(x_tsibble), x_ts)

#' ### Use same functions for all time series classes
#'
#' tsbox provides a basic toolkit for handling time series. These functions
#' start with `ts_`, so you use them with auto complete (press Tab). These
#' functions work with any *ts-boxable* time series, ts, xts, data.frame,
#' data.table tibble, zoo, tsibble or timeSeries and **return the class of their
#' inputs**.
#'
#' For example, the `ts_scale` function performs *normalization* - it subtracts
#' the mean and divides by the standard deviation of series. Like almost all ts-
#' functions, it can be used with on any ts-boxable object, with single or
#' multiple time series. Because `ts_scale` normalizes time series, it is useful
#' to make different time series comparable. All of the following operations
#' perform the same task, but return the same object class as the input:

x_scale <- ts_scale(x_ts)
ts_scale(x_xts)
ts_scale(x_df)
ts_scale(x_dt)
ts_scale(x_tbl)

#' There is a bunch of other transformation functions: `ts_trend`, which
#' estimates a trend; functions to calculate differences, `ts_pc`, `ts_pcy`,
#' `ts_diff`, `ts_diffy`; a function to shift series, `ts_lag`; functions to
#' construct indices, both from levels and percentage change rates: `ts_index`
#' and `ts_compound`. For a full list of functions, check out the reference.

ts_plot(AirPassengers)
ts_plot(ts_trend(AirPassengers))
ts_plot(ts_pc(AirPassengers))
ts_plot(ts_pcy(AirPassengers))
ts_plot(ts_index(AirPassengers))

#' ### Combine multiple time series
#'
#' A set of helper functions makes it easy to combine multiple time series, even
#' if their classes are different. The basic workhorse is `ts_c`, which simply
#' collect time series. Again, this works with single or multiple series of any
#' ts-boxable class:

ts_c(ts_tbl(EuStockMarkets), AirPassengers, ts_xts(mdeaths))

#' If you want to choose a different name for single series, name the arguments:

ts_c(ts_tbl(EuStockMarkets), `Airline Passengers` = AirPassengers)

#' Multiple series can be also combined to a single series:

ts_bind(ts_tbl(mdeaths), AirPassengers)

#' `ts_chain` offers an alternative way to combine time series, by *chain-
#' linking* them. The following prolongs a short time series with percentage
#' change rates of a longer one:

mdeaths_short <- ts_span(mdeaths, end = "1976-12-01")
mdeaths_extrapol <- ts_chain(mdeaths_short, fdeaths)

#' ### Frequency conversion and alignment
#'
#' There are functions to convert the frequency of time series and to regularize
#' irregular time series. The following changes the frequency of two series to
#' annual:

ts_frequency(ts_c(AirPassengers, austres), "year", sum) %>% head()

#' We already met `ts_span`, which can be used to limit the time span of a
#' series. `ts_regular` makes irregular time series regular, by turning implicit
#' missing values into explicit `NA`s.

ts_span(AirPassengers, -1)
ts_span(AirPassengers, start = "-3 months")
ts_span(AirPassengers, start = "-200 days")


#' ### Shortcuts for pivot_wider and pivot_longer

x_ts <- ts_c(mdeaths, fdeaths)
x_tbl <- ts_tbl(x_ts)

x_wide <- ts_wide(x_tbl)
ts_long(x_wide)


#' ### And plot just about everything
#'
#' Of course, this works for plotting, too. The basic function is `ts_plot`,
#' which can be used with any ts-boxable time series, single or multiple, of any
#' frequency:

ts_plot(AirPassengers, ts_df(lynx), ts_xts(fdeaths))

#' If you want to use different names than the object names, just name the
#' arguments (and optionally set a title):

ts_plot(
  `Airline Passengers` = AirPassengers,
  `Lynx trappings` = ts_df(lynx),
  `Deaths from Lung Diseases` = ts_xts(fdeaths),
  title = "Airlines, trappings, and deaths",
  subtitle = "Monthly passengers, annual trappings, monthly deaths"
)

#' There is also a version that uses
#' [ggplot2](https://CRAN.R-project.org/package=ggplot2) that uses the same
#' syntax. With `theme_tsbox()` and `scale_color_tsbox()`, the output of
#' `ts_ggplot` is very similar to `ts_plot`.

ts_ggplot(
  ts_scale(
    ts_c(
      mdeaths,
      austres,
      AirPassengers,
      DAX = EuStockMarkets[ ,'DAX']
    )
  )
) +
theme_tsbox() +
scale_color_tsbox()


#' ### Using tsbox in a dplyr / pipe workflow
#'
#' tsbox works well with tibbles and with `%>%`, so it can be nicely integrated
#' into a tidyverse workflow:
#'
library(tidyverse)
library(nycflights13)
dta <- weather %>%
  select(origin, time = time_hour, temp, humid, precip) %>%
  ts_long()

dta %>%
  filter(id == "temp") %>%
  ts_plot()


#' ## Exercises
#'
#' 1. Combine `fdeaths` and `mdeaths` to a multiple time series. Call it
#'    `tdeaths`
#'
#' 2. Convert them first to a long tibble, then to a wide tibble.
#'
#' 3. Plot `fdeaths` and `mdeaths`, using `ts_plot()`. Make sure to use some
#'    nice labels for the series in the legend.
#'
#' 4. Use `ts_trend()` to estimate a trend line of the two series. Include it
#'    in the same graph.
#'
#' # Data Transformation 4
#'
#' [Intro Slides](11-transformation-4.html)

