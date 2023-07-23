FROM rockylinux/rockylinux:8
LABEL maintainer 'Jeff Ohrstrom <johrstrom@osc.edu>'

ENV R_BASE_VERSION 4.0.3
ENV CRAN https://cran.case.edu/

RUN dnf update -y && dnf clean all && rm -rf /var/cache/dnf/*
RUN dnf install -y \
        dnf-utils \
        epel-release \
    && dnf config-manager --set-enabled powertools \
    && dnf clean all && rm -rf /var/cache/dnf/*
RUN dnf install -y \
        gcc gcc-c++ gcc-gfortran gdb make curl curl-devel openssl-devel libxml2-devel libjpeg-turbo-devel \
        udunits2-devel cairo-devel proj-devel sqlite-devel geos-devel gdal gdal-devel \
        readline-devel libXt-devel java-11-openjdk-devel doxygen doxygen-latex texlive \
        freetype-devel libpng-devel libtiff-devel harfbuzz-devel fribidi-devel cmake R \
        sssd \
    && dnf clean all && rm -rf /var/cache/dnf/*

# cairo is available, but sadly not found first, so force to find it.
# RUN echo "options(bitmapType='cairo')" >/usr/lib64/R/etc/Rprofile.site
RUN echo "options(repos=structure(c(CRAN='${CRAN}')))" >> /usr/lib64/R/etc/Rprofile.site

RUN curl --fail -sSLo /etc/yum.repos.d/passenger.repo https://oss-binaries.phusionpassenger.com/yum/definitions/el-passenger.repo && \
    dnf install -y passenger && \
    dnf clean all && rm -rf /var/cache/dnf/* && \
    passenger-config validate-install

# build all the dependencies before you copy the app to cache these layers
RUN Rscript -e "install.packages('devtools')"
RUN chmod 1777 /tmp

COPY . /app
RUN cd /app; LANG="C.UTF-8" Rscript -e "library('devtools'); install()"

WORKDIR /app
