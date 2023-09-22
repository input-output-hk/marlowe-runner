# Docker Image for Marlowe Runtime
#
# Execute `docker build --tag marlowe-runner.` from this folder to build this
# docker image.
# 
# To run the image, specify both the `MARLOWE_WEB_SERVER_URL` for the Marlowe
# Runtime web server instance and the port (default 8080) where the container
# exposes Marlower Runner. For example,
#   podman run -p 8123:8080 \
#     -e MARLOWE_WEB_SERVER_URL=http://192.168.0.12:13780 \
#     15f44f95c17182fa8042b55991cbf77f6d6ff8d096c34d91d8c64a49ff8733b6

LABEL description="Marlowe Runner"

FROM nixos/nix:2.13.5

RUN nix-channel --update

RUN git clone https://github.com/input-output-hk/marlowe-runner -b PLT-7536

RUN mkdir .cache

WORKDIR marlowe-runner

RUN nix develop \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --accept-flake-config \
  --command bash -c "spago build"

ENV MARLOWE_WEB_SERVER_URL=http://host.containers.internal:3780

EXPOSE 8080

CMD nix develop \
  --extra-experimental-features nix-command \
  --extra-experimental-features flakes \
  --accept-flake-config \
  --command bash -c "export MARLOWE_WEB_SERVER_URL; npm run start"
