library(dplyr)
library(forcats)
library(scales)
library(labelled)
library(stringr)
library(ggplot2)
library(ggstats)
library(gridExtra)

### stacked Bar graphs and density/box plots
stacked_plot <- function(df, variable, fill_var, facet_var=NULL, facet_wrap=FALSE, title_label = NULL, legend_label = NULL,
                         facet_ncol = 4, rotate_axis=FALSE, box_plot=TRUE, legend=TRUE, nrow_legend = 1,
                         ncol_legend = NULL, y_axis_breaks = 10, expand_yaxis = c(0.01,0.05), fill_colour_label_wrap_width = 30, 
                         title_label_wrap_width = 35, text_label = TRUE, text_angle = 0, text_label_size = 3,
                         x_axis_label_wrap_width = 10, x_axis_breaks = 10, bar_width = 0.9) {
    df <- (df %>%
             dplyr::mutate(across(where(is.character), forcats::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), forcats::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[variable]]) & !is.na(df[[fill_var]]) & !is.na(df[[facet_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[variable]]
      
      label_index <- if (is.null(labelled::var_label(index))) {variable
        } else {
      labelled::var_label(index)
        }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
        } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
                        )
          }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
          p1 +
          geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
          geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                    size=text_label_size, fontface = "bold") +
          guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
            }
          } else {
            if (text_label == TRUE) {
              p1 +
                geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
                geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                          size=text_label_size, fontface = "bold")
            } else {
              p1 +
                geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
              }
          }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
          
        } else {
          p4 <- if (box_plot == TRUE) {
            p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
                coord_flip() 
            } else {
              ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
            }
            p6 <- if (legend == TRUE) { 
              p5 +
                geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
                guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
                theme(
                  axis.text.x = element_blank()
                )
              } 
            else {
              p5 +
                geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
            }
            
            } else {
              ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
                geom_density(alpha=0.4, show.legend = TRUE) +
                scale_x_continuous(n.breaks = x_axis_breaks) +
                guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
              }
          p7 <- p4 +
            scale_y_continuous(n.breaks = y_axis_breaks)
       
          p7
        }
      p_ <- p + 
        facet_wrap(as.formula(paste("~", facet_var)), ncol = facet_ncol)
      
    } else {
      df <- df[!is.na(df[[variable]]) & !is.na(df[[fill_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[variable]]
      label_index <- if (is.null(labelled::var_label(index))) {variable
      } else {
        labelled::var_label(index)
      }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
      } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
          } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
                          )  
            }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
              theme(
                axis.text.x = element_blank()
              )
            } 
          else {
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
            }
          } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE) +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
            }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        p7
      }
     p
    }
    
    plot1 <- if (is.null(title_label) & is.null(legend_label)) {
      plot +
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
             )
    } else if (is.null(title_label) & !is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
             )
    } else if (!is.null(title_label) & is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
             )
    } else {
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
             )
    }
    
    plot1
  
}


### Combine stacked Bar graphs and density/box plots
stacked_plot_grid <- function(df, vars, fill_var, facet_var=NULL, facet_wrap=FALSE, title_label = NULL, legend_label = NULL,
                         facet_ncol = 4, rotate_axis=FALSE, box_plot=TRUE, legend=TRUE, nrow_legend = 1,
                         ncol_legend = NULL, y_axis_breaks = 10, expand_yaxis = c(0.01,0.05), fill_colour_label_wrap_width = 30,
                         title_label_wrap_width = 35, text_label = TRUE, text_angle = 0, text_label_size = 3,
                         x_axis_label_wrap_width = 10, x_axis_breaks = 10, bar_width = 0.9,
                         grid_ncol =  1, grid_nrow = NULL) {
  out <- lapply(vars, function(x){
    df <- (df %>%
             dplyr::mutate(across(where(is.character), forcats::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), forcats::as_factor))
           )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_var]]) & !is.na(df[[facet_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[x]]
      label_index <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
      } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
                        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
              theme(
                axis.text.x = element_blank()
              )
            } 
          else {
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
          }
          
        } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE) +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
        }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        
        p7
      }
      p_ <- p + 
        facet_wrap(as.formula(paste("~", facet_var)), ncol = facet_ncol)
      
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[x]]
      label_index <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
      } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
                 ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
                        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
              theme(
                axis.text.x = element_blank()
              )
            } 
          else {
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
          }
        } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE)  +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
        }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        p7
      }
      p
    }
    
    plot1 <- if (is.null(title_label) & is.null(legend_label)) {
      plot +
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
        )
    } else if (is.null(title_label) & !is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
        )
    } else if (!is.null(title_label) & is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
        )
    } else {
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
        )
    }
    
    plot1
    
  })
  out
  
  grid <- do.call(gridExtra::grid.arrange, c(out, list(ncol = grid_ncol, nrow = grid_nrow))
                  )
  
}


### List output stacked Bar graphs and density/box plots
stacked_plot_list <- function(df, vars, fill_var, facet_var=NULL, facet_wrap=FALSE, title_label = NULL, legend_label = NULL,
                              facet_ncol = 4, rotate_axis=FALSE, box_plot=TRUE, legend=TRUE, nrow_legend = 1,
                              ncol_legend = NULL, y_axis_breaks = 10, expand_yaxis = c(0.01,0.05), fill_colour_label_wrap_width = 30,
                              title_label_wrap_width = 35, text_label = TRUE, text_angle = 0, text_label_size = 3,
                              x_axis_label_wrap_width = 10, x_axis_breaks = 10, bar_width = 0.9) {
  out <- lapply(vars, function(x){
    df <- (df %>%
             dplyr::mutate(across(where(is.character), forcats::as_factor)) %>%
             dplyr::mutate(across(where(is.logical), forcats::as_factor))
    )
    plot <- if (facet_wrap == TRUE) {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_var]]) & !is.na(df[[facet_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[x]]
      label_index <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
      } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
          ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
              theme(
                axis.text.x = element_blank()
              )
          } 
          else {
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
          }
          
        } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE) +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
        }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        
        p7
      }
      p_ <- p + 
        facet_wrap(as.formula(paste("~", facet_var)), ncol = facet_ncol)
      
    } else {
      df <- df[!is.na(df[[x]]) & !is.na(df[[fill_var]]),]
      index_fill <- df[[fill_var]]
      index <- df[[x]]
      label_index <- if (is.null(labelled::var_label(index))) {x 
      } else {
        labelled::var_label(index)
      }
      
      label_index_fill <- if (is.null(labelled::var_label(index_fill))) {fill_var
      } else {
        labelled::var_label(index_fill)
      }
      
      p <- if (is.factor(index)) {
        p1 <- if (rotate_axis == TRUE) {
          ggplot(data=df, aes(x= forcats::fct_rev(index), fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                              by = forcats::fct_rev(index))
          ) +
            coord_flip() 
        } else { ggplot(data=df, aes(x=index, fill = forcats::fct_rev(forcats::fct_infreq(index_fill)),
                                     by = index)
        )  
        }
        
        p2 <- if (legend == TRUE) {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold") +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = TRUE) +
              guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) 
          }
        } else {
          if (text_label == TRUE) {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE) +
              geom_text(stat = "prop", position = position_fill(.5), color="black", angle = text_angle,
                        size=text_label_size, fontface = "bold")
          } else {
            p1 +
              geom_bar(width=bar_width, position="fill", stat="count", show.legend = FALSE)
          }
        }
        
        p3 <- p2 +
          scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_label_wrap_width)) +
          scale_y_continuous(labels = scales::percent, n.breaks = y_axis_breaks, expand = expansion(mult = expand_yaxis))
        p3
        
      } else {
        p4 <- if (box_plot == TRUE) {
          p5 <- if (rotate_axis == TRUE) {
            ggplot(data=df, aes(x = forcats::fct_rev(index_fill), y = index, colour = forcats::fct_rev(index_fill))) +
              coord_flip() 
          } else {
            ggplot(data=df, aes(x = index_fill, y = index, colour = index_fill))
          }
          p6 <- if (legend == TRUE) { 
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = TRUE) +
              guides(colour=guide_legend(nrow = nrow_legend, ncol = ncol_legend)) +
              theme(
                axis.text.x = element_blank()
              )
          } 
          else {
            p5 +
              geom_boxplot(outlier.colour = "black", outlier.shape = 1, show.legend = FALSE)
          }
        } else {
          ggplot(data=df, aes(x = as.numeric(index), fill = index_fill)) + 
            geom_density(alpha=0.4, show.legend = TRUE)  +
            scale_x_continuous(n.breaks = x_axis_breaks) +
            guides(fill=guide_legend(nrow = nrow_legend, ncol = ncol_legend))
        }
        p7 <- p4 +
          scale_y_continuous(n.breaks = y_axis_breaks)
        p7
      }
      p
    }
    
    plot1 <- if (is.null(title_label) & is.null(legend_label)) {
      plot +
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
        )
    } else if (is.null(title_label) & !is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(label_index, width = title_label_wrap_width)
        )
    } else if (!is.null(title_label) & is.null(legend_label)){
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(label_index_fill, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
        )
    } else {
      plot + 
        labs(x=NULL,y=NULL, fill = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             colour = stringr::str_wrap(legend_label, width = fill_colour_label_wrap_width),
             title = stringr::str_wrap(title_label, width = title_label_wrap_width)
        )
    }
    
    plot1
    
  })
  out
  
}