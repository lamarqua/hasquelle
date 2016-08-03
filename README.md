# hasquelle

This is a very silly haskell party-trick script for translating haskell into other languages.

## running hasqelle

First clone this repo:
```git clone https://github.com/anniecherk/hasquelle```
or
```git clone https://github.com/lamarqua/hasquelle```

Then cd into it:
```cd hasquelle```

To run a file you've written in French (.hsq) you can run hasquelle from the root directory:
```./assets/lancerHasquelle <path-to-your-file.hsq>```

To run a file you've written in Russian (.xc) you can run hasqelle from the root directory:
```./assets/запуститьХаскел <path-to-your-file.xc>```

## adding a new language

If you think you're as funny as we think we are, you can add support for your favorite language!

Just add a new dictionary file (under assets) and a new runHaskell script- just change the hardcoded dictionary name.

And hit us up with a PR!




Hasquelle was written by Adrien Lamarque & Annie Cherkaev at the Recurse Center.
