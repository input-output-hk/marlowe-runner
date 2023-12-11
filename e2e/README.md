# Marlowe Runner End-To-End testing

## Running

Run the whole test suite:
```bash
./run_tests.sh '@regression'
```

If you have `xvfb-run` installed you can also use:

```bash
./run_invisible.sh '@regression'
```

Additionally we have pretty granular set of tags in the features files. You can pick and match tests by using different tags - for example:

```bash
./run_invisible.sh '@lace-configure'
```

## Wallets

Artifacts needed to install wallets for testing are found in [artifacts](artifacts).
Currently available are *Lace* and *Nami*.

