#!/usr/bin/env sh

set -eo pipefail

export MARLOWE_WEB_SERVER_URL=https://marlowe-runtime-preprod-web.scdev.aws.iohkdev.io

spago build
webpack-cli bundle --mode=production -c webpack.js --no-watch

DOCKER_HASH=$(podman build --tag marlowe-runner . | tail -n 1)

echo 'Push to docker using the following command:'
echo '  podman push '"$DOCKER_HASH"' \'
echo '    docker://docker.io/myrepo/marlowe-runner'
echo
echo 'To run the image, specify both the `MARLOWE_WEB_SERVER_URL` for the Marlowe'
echo 'Runtime web server instance and the port (default 8080) where the container'
echo 'exposes Marlower Runner. For example,'
echo '  podman run -p 8123:8080 \'
echo '    -e MARLOWE_WEB_SERVER_URL=http://192.168.0.12:13780 \'
echo "    $DOCKER_HASH"
