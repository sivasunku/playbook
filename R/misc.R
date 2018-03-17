# > t %>% group_by(direction,openReason,closeReason) %>% summarise(Profit = sum(netP))
# # A tibble: 3 x 4
# # Groups: direction, openReason [?]
# direction openReason closeReason      Profit
# <chr>     <chr>      <chr>             <dbl>
#   1 SHORT     Case#1     Profit Booking    44456
# 2 SHORT     Case#1     SLP hit         -442700
# 3 SHORT     Case#1     TrailingSLP hit   17442