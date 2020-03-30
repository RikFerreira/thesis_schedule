library(tidyverse)
library(reshape2)
library(lubridate)
library(scales)

inicio <- dmy("01/01/2020")

df <- read.table("datasets/teste.csv",
				 header = TRUE,
				 stringsAsFactors = FALSE,
				 sep = ";",
				 allowEscapes = TRUE)

df$INICIO <- df$INICIO - 1

df <- df %>%
	mutate(START = inicio + months(INICIO),
		   END = inicio + months(INICIO) + months(DURACAO)) %>% 
	melt(measure.vars = c("START", "END")) %>%
	select(-c(INICIO, DURACAO))

plot <- ggplot(df, aes(value, reorder(TAREFA, -ID))) + 
	geom_line(size = 6, colour = "steelblue") +
	scale_x_date(breaks = c(inicio,
							inicio + months(6),
							inicio + months(12),
							inicio + months(18)),
				 position = "top",
				 expand = expansion(),
				 labels = date_format("%b/%y")) +
	labs(title = "Cronograma",
		 subtitle = "Tarefas a serem executadas por trimestre") +
	theme(panel.background = element_rect(colour = "gray8", fill = alpha(255, 0)),
		  panel.grid.major.x = element_line(colour = "gray8", size = 0.5),
		  panel.grid.major.y = element_line(colour = "gray8", size = 0.125),
		  panel.grid.minor.x = element_line(colour = "gray8", linetype = "dashed", size = 0.25),
		  panel.grid.minor.y = element_blank(),
		  plot.title = element_text(face = "bold", size = rel(2)),
		  plot.title.position = "plot",
		  axis.title = element_blank(),
		  axis.text = element_text(size = rel(1)),
		  text = element_text(family = "Roboto Condensed"))

ggsave("plots/plot.png", plot = plot, device = "png", width = 160, height = (30 + 5 * nrow(df)), units = "mm", dpi = 150)
