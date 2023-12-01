# Marlowe Runner End-To-End testing

## Running

Run the whole test suite:
```bash
./run_tests.sh '@regression'
```

We provide a shorthand for that which is `./run_regression.sh`. We have it because it can be used together with `xvfb-run` to hide the popping up browser window:

```bash
xvfb-run --server-args="-screen 0, 1024x768x24" ./run_regression.sh
```

Additionally we have pretty granular set of tags in the features files. You can pick and match tests by using different tags - for example:

```bash
./run_tests.sh '@lace-configure'
```

## Wallets

Artifacts needed to install wallets for testing are found in [artifacts](artifacts).
Currently available are *Lace* and *Nami*.

