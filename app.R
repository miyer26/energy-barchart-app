library(dash)
library(dashHtmlComponents)
library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)

app = Dash$new()

df <- read.csv("./data/Primary-energy-consumption-from-fossilfuels-nuclear-renewables.csv")

x <- colnames(df)
df_na <- df %>% drop_na("Code") %>% rename(
    Fossil = x[4],
        Renewables = x[5] ,
        Nuclear = x[6],
    ) %>% pivot_longer(c(Fossil, Renewables, Nuclear), names_to="energy_type", values_to="percentage")

df_countries <- df_na %>% filter(Code != "OWID_WRL")
df_world <- df_na %>% filter(Code == "OWID_WRL")

#list_of_continents <- unique(df_continents %>% select(Entity)) 
list_of_countries <- unique(df_countries$"Entity")

marks_list=NULL
for (i in seq(1965, 2015, 5)){
     marks_list[[ toString(i) ]] <- toString(i)
                }

# ==============================================================================
#                            Layout for map and barchart
# ==============================================================================

app$layout(
    dbcCol(
list(
        htmlBr(),
        htmlBr(),
        
        htmlH4("Top/Bottom energy consumer nations"),
        htmlH6(
            "Select the number of countries to view in the bar plot using the input tab, then select whether to view to the top or bottom consumers. The plot has an hover option to view the percentage if the text is too small.",
            style=list("font-size" = "15px")
        ),
        dccSlider(
            id="tab1-year-slider",
            min=1965,
            max=2015,
            step=1,
            value=2015,
            marks=marks_list,
            tooltip= list("placement" = "top", "always_visible" = T),
            updatemode="drag"
        ),
        htmlBr(),
        dbcRow(list(
                dbcCol(
                    list(
                        htmlH4(
                            "Number of countries",
                            style=list("font-size" = "20px")
                        ),
                        htmlBr(),
                        dbcInput(
                            id="tab1-input-topN",
                            value=10,
                            type="number",
                            debounce=T,
                            required=T,
                            minlength=1
                        )
                       )
                       ),
                dbcCol(
                    list(
                         htmlH4(
                             "Ranking type",
                             style= list("font-size" = "20px")
                        ),
                        htmlBr(),
                        dccRadioItems(
                            id="tab1_top_bot",
                            options = list(
                                list("label" = "Top", "value" = "Top"), 
                                list("label" = "Bottom", "value" = "Bottom")
                                ),
                            value="Top",
                            inline=T,
                            labelStyle=list(
                                "margin-right" = "10px",
                                "margin-top" = "1px",
                                "display" = "inline-block",
                                "horizontal-align" = ""
                                )   
                        )
                    ),
                    style=list(
                       "padding" = "0px 0px 0px 500px"
                       )
                    
                )
        )
        ),
        htmlBr(),
        dccGraph(id="tab1-barchart")

)))

 # ==============================================================================
#                            Top N countries barchart
# ==============================================================================


app$callback(
    output("tab1-barchart", "figure"),
    list(
    #input("tab1-energy-type-dropdown", "value"),
    input("tab1-year-slider", "value"),
    input("tab1-input-topN", "value"),
    input("tab1_top_bot", "value")
    ),
    function(year, topN, top_bot) {
    
    if (top_bot == "Top"){
        df_sorted <- df_countries %>% filter(
            Year == year & energy_type == "Nuclear") %>% arrange(desc(percentage)) %>% slice_max(order_by= percentage, n=topN)

    } else if (top_bot == "Bottom") {
        
       df_sorted <- df_countries %>% filter(
            Year == year & energy_type == "Nuclear") %>% arrange(desc(percentage)) %>% slice_min(order_by= percentage, n=topN, with_ties=F)

        }
    
    print(length(df_sorted))

    bar_chart <- ggplot(
        df_sorted,
        aes(x=percentage,
            y=reorder(Entity, -percentage),
           fill=percentage)) + 
    geom_bar(stat='identity') +
    geom_text(aes(label = round(percentage, 1)), hjust = 0.5, colour = "black") +
    xlim(0,100) + 
    labs(x="Percentage %",
     y="Country") + 
    scale_fill_distiller(palette= "Greens", 
    limits = c(0, 100)) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 100))

    if (top_bot == "Top"){
       bar_chart <- bar_chart + ggtitle(paste0("Top ", topN, " ", "Nuclear ", "Energy Consumers in ", year))

    } else if (top_bot == "Bottom"){
        
      bar_chart <- bar_chart + ggtitle(paste0("Bottom ", topN, " ", "Nuclear ", "Energy Consumers in ", year))
    }
    
    ggplotly(bar_chart)

    }

    
)




app$run_server(host = '0.0.0.0') 

