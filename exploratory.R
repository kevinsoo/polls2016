load(file="polls.Rda")

# national polls over time
polls %>% filter(Type=="Raw", State=="U.S.") %>% 
    ggplot(aes(x=End, y=Level, color=Candidate, group=Candidate)) +
    geom_point(aes(color=Candidate), alpha=0.1) +
    # facet_wrap(~State, ncol=10) +
    stat_smooth(method=lm) +
    scale_color_manual(values=c("#3399FF", "#FF3333")) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b %Y") +
    ggtitle("National-level polls") +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5))

# distribution of national polls over time
polls %>% filter(Type=="Raw", State=="U.S.") %>% 
    ggplot(aes(x=End, y=Level, color=Candidate, group=Candidate)) +
    geom_point(aes(color=Candidate), alpha=0.1) +
    # facet_wrap(~State, ncol=10) +
    stat_smooth(method=lm, linetype="dashed", size=.8) +
    geom_hline(data=filter(election, State=="U.S.", Candidate!="Others"), aes(yintercept=Level, condition=Candidate, color=Candidate)) +    
    scale_color_manual(values=c("#3399FF", "#FF3333")) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b %Y") +
    ggtitle("National-level polls") +
    ylim(c(0,60)) +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5))

# distribution of error
polls %>% filter(Type=="Raw", State=="U.S.") %>% 
    ggplot(aes(x=Error, group=Candidate, color=Candidate)) +
    geom_bar(stat="count")

# distribution of error over time
polls %>% filter(Type=="Raw", State=="U.S.") %>% 
    ggplot(aes(x=End, y=Error, color=Candidate, group=Candidate)) +
    geom_point(aes(color=Candidate), alpha=0.1) +
    # facet_wrap(~State, ncol=10) +
    stat_smooth(method=lm, linetype="dashed", size=.8) +
    geom_hline(yintercept=0) +    
    scale_color_manual(values=c("#3399FF", "#FF3333")) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b %Y") +
    ggtitle("National-level polls") +
    theme(axis.text.x  = element_text(angle=45, vjust=0.5))
