# Hox

WIP GTK Haskell application implementation of Magic the Gathering's first set.

Decklists should be in the form of "4 AncestralRecall" and only cards from the Alpha set are recognized
Alpha set of Magic: https://scryfall.com/sets/lea?as=grid&order=set
Magic Rules: https://magic.wizards.com/en/rules

# Installation

## Debian

1. Install stack
2. stack run

3. 
sudo apt-get install libglib2.0-dev libpango1.0-dev libgtk2.0-dev
Find filepath with locate cairo | grep '\.pc'
export PKG_CONFIG_PATH #filepath
stack build
stack run

## MacOS

1. Install Stack
2. Install gtk+3 : https://macappstore.org/gtk3/
3. stack build
4. stack run
