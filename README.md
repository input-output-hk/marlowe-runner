# Marlowe Runner

A simple web app which allows you to deploy and progress through Marlowe contracts on Cardano easily from the browser.

## Installation

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

## Running

To start the server you can use the `npm run start` command, for example:
```bash
NETWORK="preview" MARLOWE_WEB_SERVER_URL="http://localhost:3780" npm run start
```

In the previous example:
- `NETWORK`: Specifies the Cardano network to use. In the example above, it's set to `preview`.
- `MARLOWE_WEB_SERVER_URL`: Specifies URL for the Marlowe Runtime Web server of the network specified (`NETWORK`).

After this, the Marlowe Runner instance should be available by default at: `http://localhost:8080/`

### Troubleshooting

Ensure that:
  -  The protocol (`http` or `https`) is correct.
  -  There's no extra `/` at the end of the URL.

If you get a blank page when opening Marlowe Runner in your browser, it may be necessary that you install a light wallet like [Nami](https://namiwallet.io/).

## Contributing

Before contributing, please format the source code using the following command from within the `nix` development shell:

```bash
purs-tidy --format-in-place src/**/*.purs
```
