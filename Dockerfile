# Docker Image for Marlowe Runtime

LABEL description="Marlowe Runner"

FROM alpine:3.16.2

RUN apk add darkhttpd \
    && mkdir marlowe-runner

COPY public/ marlowe-runner/

WORKDIR marlowe-runner

ENV MARLOWE_WEB_SERVER_URL=http://host.containers.internal:3780

EXPOSE 8080

CMD sed -i -e "s|https://marlowe-runtime-preprod-web.scdev.aws.iohkdev.io|$MARLOWE_WEB_SERVER_URL|g" bundle.js \
    && darkhttpd /marlowe-runner --addr 0.0.0.0 --port 8080
