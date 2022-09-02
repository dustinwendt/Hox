# Hox


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
2. Install GTK dependencies
    brew install pango glib cairo gtk
3. stack build
4. stack run
