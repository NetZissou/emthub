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
        R-Rcpp-devel R-Rcpp libgit2-devel R-littler-examples \
    && dnf clean all && rm -rf /var/cache/dnf/*
# Get helper script into PATH
RUN mv /usr/lib64/R/library/littler/examples/install2.r /usr/bin/install2.r

# cairo is available, but sadly not found first, so force to find it.
# RUN echo "options(bitmapType='cairo')" >/usr/lib64/R/etc/Rprofile.site
RUN echo "options(repos=structure(c(CRAN='${CRAN}')))" >> /usr/lib64/R/etc/Rprofile.site

# No aarch/arm64 packages so have to install from source on non-x86_64
RUN if [ $(uname -p) == "x86_64" ] ; then \
    curl --fail -sSLo /etc/yum.repos.d/passenger.repo https://oss-binaries.phusionpassenger.com/yum/definitions/el-passenger.repo && \
    dnf install -y passenger && \
    dnf clean all && rm -rf /var/cache/dnf/* && \
    passenger-config validate-install ; fi
RUN if [ $(uname -p) == "aarch64" ] ; then \
    dnf module enable ruby:3.0 -y && \
    dnf install -y ruby ruby-devel && \
    dnf clean all && rm -rf /var/cache/dnf/* && \
    gem install --no-document  passenger -v 6.0.18 && \
    passenger-config build-native-support && \
    passenger-config install-standalone-runtime --auto --engine nginx && \
    ln -s /usr/local/bin/passenger /bin/passenger ; fi

# Package needed by install2.r
RUN Rscript -e "install.packages('docopt')"
# build all the dependencies before you copy the app to cache these layers
RUN install2.r --error devtools

RUN chmod 1777 /tmp
COPY . /app
ENV LANG=C.UTF-8
ENV GEOCODIO_API_KEY='6ceabeead9d332ede6ab2d060b39e66209aa6ab'
ENV LOCATIONIQ_API_KEY='pk.b484b0c3b982913db4382afd1830d4ef'
ENV OPENCAGE_KEY=9'e3696d2a85443fbb8e55b20859bc1e5'
ENV MAPQUEST_API_KEY='BALT5z6bd3sAbkTlkfCyIMQitWFy9GNm'
ENV GEOAPIFY_KEY='717a18ca415e47a1bccf1a87d2cf65e5'
ENV HERE_API_KEY='Z8_5_dXXR34Q0EguapZ1JjejznAiZ0VGjIif5tJ1bsg'
RUN cd /app; Rscript -e "library('devtools'); install()"

RUN groupadd -g 8052 PAS2531
RUN useradd -g PAS2531 -u 42168 -m emthub
RUN chown -R emthub: /app

WORKDIR /app
