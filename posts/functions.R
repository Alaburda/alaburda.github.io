generate_intervals <- function(date_from = "2024-07-01",
                               groups = 3,
                               duration = 10,
                               gaps = 1:2,
                               n = 10) {

  data.frame(id = 1:(n*groups),
             group_id = as.character(rep(1:groups,each = n))) %>%
    group_by(group_id) %>%
    mutate(duration = sample.int(duration, n, replace = TRUE),
           gap = c(0,sample(gaps, n-1, replace = TRUE)),
           date_kernel = as.Date(date_from)) %>%
    mutate(lag_duration = replace_na(lag(duration),0),
           cumulative_duration = cumsum(duration),
           cumulative_gap = cumsum(gap)) %>%
    mutate(cumulative_lag_duration = cumsum(lag_duration)) %>%
    mutate(date_from = date_kernel %m+% days(cumulative_gap+cumulative_lag_duration),
           date_to = date_kernel %m+% days(cumulative_gap+cumulative_duration))

}

plot_timeline <- function(data) {

  data_long <- data %>%
    pivot_longer(names_to = "name", values_to = "date", c(date_from, date_to))

  ggplot(data_long, aes(x = reorder(id, group_id, min), y = date, color = group_id)) +
    geom_line() +
    # scale_y_date(limits = c(as.Date("2007-01-01"),as.Date("2026-01-01")), date_breaks = "2 years", date_labels =  "%Y") +
    coord_flip() +
    geom_point(aes(color = group_id)) +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank()) +
    guides(color = FALSE)

}
