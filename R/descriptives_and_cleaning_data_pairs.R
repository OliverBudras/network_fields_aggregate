library(openalexR)
library(tidyverse)
library(here)
library(bibliometrix)
library(bibliometrixData)
library(wordcloud)
library(plotly)


### Data Load

data <- readRDS("data_pairs_filtered.RDS")

f <- data |> 
  filter(publication_year >= 1970 & type == "article") |> 
  arrange(publication_date) |> 
  head()

types <- data |> 
  group_by(type) |> 
  summarise(N = n()) |> 
  mutate(type = as.factor(type)) |> 
  ggplot(aes(N, fct_reorder(type, N))) +
  geom_bar(stat = "identity")

ggplotly(types)

### First Data Cleaning and Filtering

data_clean <- data |> 
  filter(type %in% c("article", "review") & language == "en") |>  # Filter for articles in English
  select(-apc,-license,-grants) |> # De-select items
  distinct(title, .keep_all = T) |> 
  filter(referenced_works != "NA") 

articles_over_time <- data_clean |> 
  group_by(publication_year) |> 
  summarise(N = n()) |> 
  ggplot(aes(publication_year, N)) +
  geom_bar(stat = "identity")

plotly::ggplotly(articles_over_time)


data_clean2 <- data_clean |> 
  filter(publication_year  >= 1960 & publication_year < 2025) 

year_bucket <- data.frame(publication_year = seq(min(data_clean2$publication_year), max(data_clean2$publication_year), 1)) |> 
  mutate(year_bucket_5 = cut(publication_year,
                             breaks = seq(min(publication_year), max(publication_year) + 5, by = 5),
                             right = FALSE,
                             labels = paste(seq(min(publication_year), max(publication_year), by = 5),
                                            seq(min(publication_year)+4, max(publication_year)+4, by = 5),
                                            sep = "-")),
         year_bucket_10 = cut(publication_year,
                              breaks = seq(min(publication_year), max(publication_year) + 10, by = 10),
                              right = FALSE,
                              labels = paste(seq(min(publication_year), max(publication_year), by = 10),
                                             seq(min(publication_year)+9, max(publication_year)+9, by = 10),
                                             sep = "-")))

data_final <- data_clean2 |> 
  left_join(year_bucket)

ggplotly(data_final |> 
           group_by(year_bucket_5) |> 
           summarise(N = n()) |> 
           ggplot(aes(year_bucket_5, N)) +
           geom_bar(stat = "identity"))


ggplotly(data_final |> 
           group_by(year_bucket_10) |> 
           summarise(N = n()) |> 
           ggplot(aes(year_bucket_10, N)) +
           geom_bar(stat = "identity"))

data_final_1970 <- data_final |> 
  filter(publication_year >= 1970)

######################## Extracting data items of interest

## Extracting fields

fields <- data_final_1970 |> 
  select(oa_id, topics) |> 
  unnest(cols = c(topics)) |> 
  mutate(type_no = paste0(type, "_", i)) |> 
  pivot_wider(id_cols = oa_id, names_from = type_no, values_from = display_name)

## Extracting authors

authors <- data_final_1970 |> 
  select(oa_id, authorships, cited_by_count) |> 
  unnest(cols = c(authorships)) |> 
  select(oa_id, display_name, cited_by_count) |> 
  left_join(fields) 

## Authors as strings

authors_string <- authors |> 
  group_by(oa_id) |> 
  summarise(authors = paste(display_name, collapse = ";"), .groups = "drop") 

## Extracting keywords 

keywords <- data_final_1970 |> 
  select(oa_id, keywords) |> 
  unnest(cols = c(keywords)) |> 
  select(oa_id, keywords = display_name) |> 
  drop_na(keywords)

data_final_1970 <- data_final_1970 |> 
  left_join(fields) 




data_aggregator_1V <- function(data,grouping_var){
  
  
  data_agg <- data |> 
    group_by({{grouping_var}}) |> 
    summarise(No_publications = n(),No_citations = sum(cited_by_count)) 
  
  return(data_agg)
  
}


data_aggregator_2V <- function(data,grouping_var1, grouping_var2){
  
  
  data_agg <- data |> 
    group_by({{grouping_var1}}, {{grouping_var2}}) |> 
    summarise(No_publications = n(),No_citations = sum(cited_by_count)) 
  
  return(data_agg)
  
}

descriptives <- function(data){
  
  
  ###################################### Aggregating data 
  
  N_publications <- nrow(data) # total number of publications
  start_year <- min(data$publication_year) # starting year
  end_year <- max(data$publication_year) # end year
  average_citations <- mean(data$cited_by_count) # average citations
  N_authors <- authors |> distinct(display_name) |> nrow() # Number of authors
  articles_per_author <- authors |> group_by(display_name) |> summarise(N=n()) |> ungroup() |> summarise_at(vars(N),mean) # average number of articles per author
  author_per_articles <- authors |> group_by(oa_id) |> summarise(N=n()) |> ungroup() |> summarise_at(vars(N),mean) # average number of authors per article
  
  N_publications_over_time <- data_aggregator_1V(data, grouping_var = publication_year) # publication/citations over time
  
  top_articles <- data |> 
    select(oa_id, title, cited_by_count, publication_year) |> 
    left_join(authors_string) |> 
    left_join(fields) |> 
    slice_max(n = 20, order_by = cited_by_count) |> 
    select(authors,publication_year, title, cited_by_count, field_1) # top articles by citations
  
  
  articles_per_domain <- data_aggregator_1V(data, grouping_var = domain_1) |> drop_na(domain_1) # publication/citations per domain
  
  
  articles_per_field <- data_aggregator_1V(data, grouping_var = field_1) |> drop_na(field_1) # publications/citations per field
  
  
  
  N_publications_over_time_domain <- data_aggregator_2V(data, publication_year, domain_1) |> drop_na() # publications/citations per domain over time
  
  
  N_publications_over_time_field <- data_aggregator_2V(data, publication_year, field_1) |> drop_na() # publications/citations per field over time
  
  
  
  
  top_authors <- data_aggregator_1V(authors, display_name) # publications/citations per author
  
  
  top_authors_domain <- data_aggregator_2V(authors, display_name, domain_1) # publications/citations per author and domain
  
  
  
  top_authors_field <- data_aggregator_2V(authors, display_name, field_1) # publications/citations per author and field
  
  
  
  top_journals <- data_aggregator_1V(data, source_display_name) |> drop_na(source_display_name) # publications/citations per journal
  
  
  keywords_prep <- keywords |> 
    group_by(keywords) |> 
    summarise(N = n()) |> 
    arrange(desc(N)) # number of articles with keyword i
  
  
  
  
  ################################# Generating Tables
  
  description_table <- tibble(data = c(N_publications,start_year,end_year,round(average_citations,2), N_authors,round(articles_per_author[1,1],2),
                                       round(author_per_articles[1,1],2))) |> 
    mutate(Description = c("Articles", "Year (Begin)", "Year (End)", "Average Citations per Article", "Authors",
                           "Articles per Author", "Authors per Article")) |> 
    select(Description, everything()) |> 
    mutate(data = as.numeric(data))
  
  colnames(description_table)[2] <- ""
  
  N_publications_over_time_domain_tbl <- N_publications_over_time_domain |> 
    pivot_wider(id_cols = publication_year, names_from = domain_1, values_from = No_publications) |> 
    mutate(across(everything(), ~ replace(., is.na(.), 0)))
  
  N_citations_over_time_domain_tbl <- N_publications_over_time_domain |> 
    pivot_wider(id_cols = publication_year, names_from = domain_1, values_from = No_citations) |> 
    mutate(across(everything(), ~ replace(., is.na(.), 0)))
  
  N_publications_over_time_field_tbl <- N_publications_over_time_field |> 
    pivot_wider(id_cols = publication_year, names_from = field_1, values_from = No_publications) |> 
    mutate(across(everything(), ~ replace(., is.na(.), 0)))
  
  N_citations_over_time_field_tbl <- N_publications_over_time_field |> 
    pivot_wider(id_cols = publication_year, names_from = field_1, values_from = No_citations) |> 
    mutate(across(everything(), ~ replace(., is.na(.), 0)))
  
  
  top_authors_publications_tbl <- top_authors |> 
    arrange(desc(No_publications)) |> 
    slice_max(order_by = No_publications, n = 50) |> 
    select(author=display_name, No_publications)
  
  
  top_authors_citations_tbl <- top_authors |> 
    arrange(desc(No_citations)) |> 
    slice_max(order_by = No_citations, n = 50) |> 
    select(author = display_name, No_citations)
  top_authors_citations_tbl <- top_authors_citations_tbl[1:50,]
  
  
  top_authors_domain_publications_tbl <- top_authors_domain |> 
    group_by(domain_1) |> 
    slice_max(order_by = No_publications, n=5, with_ties = F) |> 
    drop_na(domain_1) |> 
    mutate(row =  row_number()) |> 
    mutate(author_info = paste0(display_name, " (", No_publications, ")")) |> 
    pivot_wider(id_cols = row,names_from = domain_1, values_from = author_info) |> 
    select(-row)
  
  
  top_authors_domain_citations_tbl <- top_authors_domain |> 
    group_by(domain_1) |> 
    slice_max(order_by = No_citations, n=5, with_ties = F) |> 
    drop_na(domain_1) |> 
    mutate(row =  row_number()) |> 
    mutate(author_info = paste0(display_name, " (", No_citations, ")")) |> 
    pivot_wider(id_cols = row,names_from = domain_1, values_from = author_info) |> 
    select(-row)
  
  
  top_authors_field_publications_tbl <- top_authors_field |> 
    group_by(field_1) |> 
    slice_max(order_by = No_publications, n=5, with_ties = F) |> 
    drop_na(field_1) |> 
    mutate(row =  row_number()) |> 
    mutate(author_info = paste0(display_name, " (", No_publications, ")")) |> 
    pivot_wider(id_cols = row,names_from = field_1, values_from = author_info) |> 
    select(-row)
  
  
  top_authors_field_citations_tbl <- top_authors_field |> 
    group_by(field_1) |> 
    slice_max(order_by = No_citations, n=5, with_ties = F) |> 
    drop_na(field_1) |> 
    mutate(row =  row_number()) |> 
    mutate(author_info = paste0(display_name, " (", No_citations, ")")) |> 
    pivot_wider(id_cols = row,names_from = field_1, values_from = author_info) |> 
    select(-row)
  
  
  
  top_journals_publications_tbl <- top_journals |> 
    arrange(desc(No_publications)) |> 
    slice_max(order_by = No_publications, n = 50) |> 
    select(journal=source_display_name, No_publications)
  
  
  top_journals_citations_tbl <- top_journals |> 
    arrange(desc(No_citations)) |> 
    slice_max(order_by = No_citations, n = 50) |> 
    select(journal = source_display_name, No_citations)
  top_journals_citations_tbl <- top_journals_citations_tbl[1:50,]
  
  
  
  keywords_tbl <- keywords_prep |> 
    slice_head(n=20) 
  
  
  
  ################################# Generating Plots
  
  
  N_publications_over_time_plt <- ggplot(N_publications_over_time, aes(publication_year, No_publications)) +
    geom_line() +
    labs(y="Publicatiions", x="Year") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  N_references_over_time_plt <- ggplot(N_publications_over_time, aes(publication_year, No_citations)) +
    geom_line()+
    labs(y="Citations", x="Year") +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  N_over_time_plt <- gridExtra::grid.arrange(N_publications_over_time_plt, N_references_over_time_plt)
  
  
  articles_per_domain_plt <- ggplot(articles_per_domain,aes(No_publications, domain_1)) +
    geom_bar(stat = "identity") +
    labs(y = "Domain", x="Publications") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,30000))
  
  articles_per_domain_plt2 <- ggplot(articles_per_domain,aes(No_citations, domain_1)) +
    geom_bar(stat = "identity") +
    labs(y = "Domain", x="Citations") +
    theme_bw() +
    theme(panel.grid = element_blank())  +
    scale_x_continuous(expand = c(0,0), limits = c(0, 800000))
  
  domain_plt <- gridExtra::grid.arrange(articles_per_domain_plt,articles_per_domain_plt2)
  
  
  
  articles_per_field_plt <- ggplot(articles_per_field,aes(No_publications, fct_reorder(field_1, No_publications))) +
    geom_bar(stat = "identity")  +
    labs(y = "Field", x="Publications") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,15000))
  
  articles_per_field_plt2 <- ggplot(articles_per_field,aes(No_citations, fct_reorder(field_1, No_citations))) +
    geom_bar(stat = "identity")  +
    labs(y = "Domain", x="Citations") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,300000))
  
  field_plt <- gridExtra::grid.arrange(articles_per_field_plt,articles_per_field_plt2)
  
  
  
  
  N_publications_over_time_domain_plt <- N_publications_over_time_domain |> 
    rename(Domain = domain_1) |> 
    ggplot(aes(publication_year,No_publications, color=Domain)) +
    geom_line() +
    labs(x = "Year", y="Publications") +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "bottom") +
    scale_x_continuous(expand = c(0,0), limits = c(1970,2025))
  
  N_citations_over_time_domain_plt <- N_publications_over_time_domain |> 
    rename(Domain = domain_1) |> 
    ggplot(aes(publication_year,No_citations, color=Domain)) +
    geom_line() +
    labs(x = "Year", y="Citations") +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.position = "bottom") +
    scale_x_continuous(expand = c(0,0), limits = c(1970,2025))
  
  N_publications_over_time_field_plt <- N_publications_over_time_field |> 
    ggplot(aes(publication_year,No_publications, color=field_1)) +
    geom_line()+
    theme_bw()  +
    theme(legend.position = "bottom") + theme(
      legend.text = element_text(size = 7),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "lines"),  # shrink boxes
      panel.grid = element_blank())  +
    scale_x_continuous(expand = c(0,0), limits = c(1970,2025)) +
    labs(x = "Year", y="Publications") 
  
  N_citations_over_time_field_plt <- N_publications_over_time_domain |> 
    ggplot(aes(publication_year,No_citations, color=field_1)) +
    geom_line()+
    theme_bw()  +
    theme(legend.position = "bottom") + theme(
      legend.text = element_text(size = 7),
      legend.title = element_blank(),
      legend.key.size = unit(0.5, "lines"),  # shrink boxes
      panel.grid = element_blank())  +
    scale_x_continuous(expand = c(0,0), limits = c(1970,2025)) +
    labs(x = "Year", y="Citations") 
  
  
  top_authors_publications_plt <- top_authors_publications_tbl |> 
    ggplot(aes(No_publications,fct_reorder(author, No_publications))) +
    geom_bar(stat = "identity") +
    labs(y="Author", x="Publications") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,175))
  
  
  top_authors_citations_plt <- top_authors_citations_tbl |> 
    ggplot(aes(No_citations,fct_reorder(author, No_citations))) +
    geom_bar(stat = "identity") +
    labs(y="Author", x="Citations") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,25000))
  
  top_authors_plt <- gridExtra::grid.arrange(top_authors_publications_plt, top_authors_citations_plt)
  
  top_journals_publications_plt <- top_journals |> 
    arrange(desc(No_publications)) |> 
    slice_max(order_by = No_publications, n = 50) |> 
    ggplot(aes(No_publications,fct_reorder(source_display_name, No_publications))) +
    geom_bar(stat = "identity") +
    labs(y="Journal", x="Publications") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,2500))
  
  top_journals_citations_plt <- top_journals |> 
    arrange(desc(No_citations)) |> 
    slice_max(order_by = No_citations, n = 50) |> 
    ggplot(aes(No_citations,fct_reorder(source_display_name, No_citations))) +
    geom_bar(stat = "identity") +
    labs(y="Journal", x="Citations") +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,50000))
  
  top_journals_plt <- gridExtra::grid.arrange(top_journals_publications_plt, top_journals_citations_plt)
  
  set.seed(123)  # for reproducibility
  wc <- wordcloud(
    words = keywords_prep$keywords,
    freq = keywords_prep$N,
    #min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.2,
    colors = brewer.pal(8, "Dark2")
  )
  
  wc_record <- recordPlot(wc)
  
  ################################## Generating the list of tables/plots
  
  list(description_table = description_table,
       N_publications = N_publications,
       N_over_time_tbl = N_publications_over_time,
       N_over_time_plt = N_over_time_plt,
       top_articles = top_articles,
       articles_per_domain = articles_per_domain,
       articles_per_field = articles_per_field,
       domain_plt = domain_plt,
       field_plt = field_plt,
       N_publications_over_time_domain_tbl = N_publications_over_time_domain_tbl,
       N_publications_over_time_field_tbl = N_publications_over_time_field_tbl,
       N_citations_over_time_domain_tbl = N_citations_over_time_domain_tbl,
       N_citations_over_time_field_tbl = N_citations_over_time_field_tbl,
       N_publications_over_time_domain_plt = N_publications_over_time_domain_plt,
       N_publications_over_time_field_plt = N_publications_over_time_field_plt,
       N_citations_over_time_domain_plt = N_citations_over_time_domain_plt,
       N_citations_over_time_field_plt  = N_citations_over_time_field_plt,
       top_authors_publications_tbl = top_authors_publications_tbl,
       top_authors_citations_tbl = top_authors_citations_tbl,
       top_authors_plt = top_authors_plt,
       top_authors_domain_publications_tbl = top_authors_domain_publications_tbl,
       top_authors_domain_citations_tbl=top_authors_domain_citations_tbl,
       top_authors_field_publications_tbl=top_authors_field_publications_tbl,
       top_authors_field_citations_tbl=top_authors_field_citations_tbl,
       top_journals_publications_tbl = top_journals_publications_tbl,
       top_journals_citations_tbl=top_journals_citations_tbl,
       top_journals_plt=top_journals_plt,
       keywords_prep = keywords_prep,
       keywords_tbl=keywords_tbl,
       wc=wc_record)
  
  
  
}

descriptives_list <- descriptives(data_final_1970)


descriptives_list$description_table
descriptives_list$N_over_time_tbl
descriptives_list$top_articles
descriptives_list$articles_per_domain
descriptives_list$articles_per_field
descriptives_list$N_publications_over_time_domain_tbl
descriptives_list$N_publications_over_time_field_tbl
descriptives_list$N_citations_over_time_domain_tbl
descriptives_list$N_citations_over_time_field_tbl
descriptives_list$top_authors_publications_tbl
descriptives_list$top_authors_citations_tbl
descriptives_list$top_authors_domain_publications_tbl
descriptives_list$top_authors_domain_citations_tbl
descriptives_list$top_authors_field_publications_tbl
descriptives_list$top_authors_field_citations_tbl
descriptives_list$top_journals_publications_tbl
descriptives_list$top_journals_citations_tbl
descriptives_list$keywords_tbl
descriptives_list$N_over_time_plt |> plot()
descriptives_list$domain_plt |> plot()
descriptives_list$field_plt |> plot()
descriptives_list$N_publications_over_time_domain_plt |> plot()
descriptives_list$N_citations_over_time_domain_plt |> plot()
descriptives_list$N_publications_over_time_field_plt |> plot()
descriptives_list$N_citations_over_time_field_plt |> plot()
descriptives_list$top_authors_plt |> plot()
descriptives_list$top_journals_plt |> plot()
descriptives_list$wc |> replayPlot()

saveRDS(descriptives_list, "descriptives.RDS")
saveRDS(data_final_1970, "data_final_1970.RDS")


