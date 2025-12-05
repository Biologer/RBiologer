# RBiologer package

RBiologer is, as you probably know, an official R package for Biologer. It is used to download data from multiple Biologer servers using your user token. It also contains various functions that we use for simple statistical analysis and data overview.

## Geting started

Use simple `biologer.login()` function to logg into the biologer server(s). For each server, you would need an user token that could be obtained through the web browser from User > Preferences > API Tokens > Generate token (or by typing https://biologer.(rs, hr, ba, me)/preferences/token).

After successfull login, you can just use the function `open_data()` that will automatically download data calling helper function `get_biologer_data()` and gave you the result as data.table. This is a relativelly simple code that you can use to get your first data:

```
biologer.login("rs", "YOUR SECRET TOKEN")
dataset_rs <- as.data.frame(open_data())
```

## Using the data

The data gathered through the `open_data()` is provided in standard DarwinCore standard and accessible by other RBiologer functions. You can use the following functions for data processing.

* `points_in_polygon(polygon = area.sf)` allows you to select only the data found in certain area (i.e. your study region).
* `filter_data_by_license()` returns only the publically available data according to the license choosen by the uses. Use this if you are an admin or editor before sharing the data to the world.