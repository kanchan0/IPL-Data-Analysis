FROM rocker/r2u:24.04

WORKDIR /app

RUN install.r \
      readr dplyr tidyr ggplot2 \
      treemap RColorBrewer \
      caret randomForest \
      scales viridis igraph reshape2 \
    && rm -rf /tmp/downloaded_packages /var/lib/apt/lists/* /root/.cache

COPY "project(IPL).R" /app/

# tidyverse is redundant: dplyr, tidyr, ggplot2 and readr are already loaded individually
RUN sed -i '/^library(tidyverse)/d' "project(IPL).R"

VOLUME ["/app/IPL", "/app/Plots"]

CMD ["Rscript", "project(IPL).R"]