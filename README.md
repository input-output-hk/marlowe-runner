# Marlowe Runner

A simple web app which allows you to deploy and progress through Marlowe contracts on Cardano easily from the browser.

## Installation

The application itself is a fully static JS app which requires Marlowe Runtime for running. A release package is providing a directory which could be served by your favorite HTTP server but in order to use it you should setup a minimal configuration.

### Configuration

In order to configure the application your HTTP server should serve a `config.json`. To do so, create the file `public/config.json` with the following structure:

```json
{ "marloweWebServerUrl": 'https://link-to-runner-instance', "develMode": false }
```


## Development

### Prerequisites

Ensure you have `nix` installed on your system and an instance of the [Marlowe Runtime](https://docs.marlowe.iohk.io/docs/getting-started/deployment-options)'s Webserver running pointed to the desired Cardano network.

### Setup

Enter the development shell:
```bash
nix develop
```

Install the necessary dependencies:
```bash
npm install
```

Run the tests:
```bash
spago test
```

## Running devel server

To start the server you can use the `npm run start` command, for example:
```bash
MARLOWE_WEB_SERVER_URL="http://localhost:3780" npm run start
```

In the previous example:
- `MARLOWE_WEB_SERVER_URL`: Specifies URL for the Marlowe Runtime Web server.

After this, the Marlowe Runner instance should be available by default at: `http://localhost:8080/`

## Bundling

If you would like to use optimzed bundle of the project you can run t:

```bash
MARLOWE_WEB_SERVER_URL="http://localhost:3780" npm run bundle
```

## Development with Nix

This repository uses nix to provide a development and build environment.

For instructions on how to install and configure nix (including how to enable access to our binary caches), refer to [this document](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md). 

If you already have nix installed and configured, you may enter the development shell by running `nix develop`.

You can build the static site with Nix using:
```nix
nix build .#marlowe-runner
```

Whenever you make changes to `package-json.lock` or `packages.dhall` packages, you must run the
`gen-nix-lockfiles` script (available inside the `nix develop` shell) to
recreate `nix/spago-packages.nix` and `nix/npm-deps-hash.nix`.

### Troubleshooting

Ensure that:
  -  The protocol (`http` or `https`) is correct.
  -  There's no extra `/` at the end of the URL.

If you get a blank page when opening Marlowe Runner in your browser, it may be necessary that you install a light wallet like [Nami](https://namiwallet.io/).

## Contributing

Before contributing, please format the source code using the following command from within the `nix` development shell:

```bash
purs-tidy format-in-place 'src/**/*.purs'
```
