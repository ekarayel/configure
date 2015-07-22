# Configure Helper

## Description
Returns parts of the cabal package description file for projects hosted on GitHub, e.g.

+ the cabal package version is the result of git describe --allways,
+ similarly the source-repository entries with kinds `head` and `this` are computed and
+ embeds the Markdown sections Synopsis and Description from the `README.md` into the package description.
