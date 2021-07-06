FROM rocker/tidyverse

COPY . .

RUN Rscript -e "install.packages(c('ggplot2','tidyverse','vip','jsonlite','glmnet'))"

EXPOSE 8080

CMD ["Rscript", "R/run_api.R"]
