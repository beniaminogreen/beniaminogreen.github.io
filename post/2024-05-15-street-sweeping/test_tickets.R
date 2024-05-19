library(readxl)
library(tidyverse)
library(sf)
library(ggrepel)

load("new_haven_sf.rda")

data <- read_excel("Street_Sweeping_Citations_Issued_Filtered_by_Date.xlsx") %>%
    rename_with(tolower) %>%
    mutate(
           latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)
           )


points <- map2(data$longitude, data$latitude, ~st_point(c(.x, .y)))
precincts <- map(points, st_within, new_haven_sf)

data$precinct_num <- precincts %>%
    map_dbl(
         ~ifelse(length(.x[[1]])==0, NA_real_, .x[[1]])
        )
data$precinct_name <- new_haven_sf$name[data$precinct_num]

data %>%
    ggplot(aes(x=longitude, y=latitude, col = precinct_name)) +
    geom_point()

data %>%
    group_by(precinct_name) %>%
    summarize(
              n = n(),
              mean_total_due_per_ticket = mean(totaldue, na.rm=T),
              mean_total_paid_amount = mean(totalpaidamount, na.rm=T)
              ) %>%
    ggplot(aes(x=mean_total_due_per_ticket, y=mean_total_paid_amount, label = precinct_name)) +
    geom_point() +
    geom_text_repel() +
    xlab("Mean amount due per ticket") +
    ylab("Mean amount paid per ticket") +
    theme_bw(base_size=20) +
    ggtitle("Mean Amount Paid / Due by Neighborhood")



# data %>%
#     select(issuedate, violationcost, totaldue, totalpaidamount) %>%
#     arrange(issuedate)  %>%
#     drop_na() %>%
#     mutate(
#            inital_value = cumsum(violationcost),
#            with_late_fees = cumsum(totaldue),
#            amount_paid = cumsum(totalpaidamount)
#            )    %>%
#     pivot_longer(c(inital_value, with_late_fees, amount_paid)) %>%
#     ggplot() +
#     geom_line(aes(x=issuedate, y=value/(10^6), col =name, linetype = name), linewidth = 2) +
#     ggtitle("Cumulative Value of Street Sweeping Tickets Issued")  +
#     xlab("Date Ticket Issued") +
#     ylab("Ammount Due (Millions)") +
#     theme_bw()
