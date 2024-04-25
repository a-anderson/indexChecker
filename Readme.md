# IndexChecker R Shiny App

## Overview

Check whether your indices can be pooled into the same DNA sequencing run.

This app compares your sample indices and tells you if they can't be sequenced together. This is determined by the Molecular Genomics Core requirement of having at least 3 different bases between indices.

You can add indices either by selecting them from the kit and set you used, or you can enter them manually. You can enter indices in upper or lower case (indexChecker converts them all to upper case automatically).

**NOTE:** If choosing indices from the selection table please select the machine that will be used to sequence your samples.

## How to run

1. Build the dockerfile using the following command from the command line:

```
docker build -t index_checker .
```

Or use this command if your device is an ARM based system (e.g. Apple M1 Mac, etc.)

```
docker build --platform linux/x86_64 -t index_checker .
```

This may take some time to build.

2. Run the container using the following command:

```
docker run -p 3838:3838 index_checker
```

3. Open your browser and paste http://localhost:3838/ into the address bar.
