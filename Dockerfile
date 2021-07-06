FROM rocker/tidyverse

COPY R/endpoint.R R/endpoint.R
COPY R/funcoes.R R/funcoes.R
COPY R/run_api.R R/run_api.R

RUN Rscript -e "install.packages('tidymodels')"
RUN Rscript -e "install.packages('vip')"
RUN Rscript -e "install.packages('jsonlite')"
RUN Rscript -e "install.packages('glmnet')"
RUN Rscript -e "install.packages('plumber')"

EXPOSE 8080

CMD ["Rscript", "R/run_api.R"]
