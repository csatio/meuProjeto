FROM rocker/tidyverse

COPY . .

RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('vip')"
RUN Rscript -e "install.packages('jsonlite')"
RUN Rscript -e "install.packages('glmnet')"

EXPOSE 8080

CMD ["Rscript", "R/run_api.R"]
