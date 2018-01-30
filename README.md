# hn-tui
Browse [Hacker News](https://news.ycombinator.com) from your Terminal

<img src="https://i.imgur.com/oHcUTzv.gif" alt="Demo Animated Gif" width="958" height="520"/>

hn-tui is a terminal user interface for browsing Hacker News written in Haskell.

# Platforms
Linux, Mac, Windows (via Windows Subsystem for Linux)

# Dependencies
Building/installing hn-tui requires the stack build tool.  It can be gotten at [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/).  You will also need to have ncurses installed.

# Install Instructions
In your terminal run the following:
``` shell
git clone https://github.com/dmille56/hn-tui.git
cd hn-tui
stack install
```

(This gets the source code with git, changes directory to the downloaded source code, and then runs stack to build/install the code).

# How to run
In your terminal run:
``` shell
hn-tui
```

# Run with a custom theme
``` shell
hn-tui --theme yourtheme.ini
```

# Build Instructions
Build:
```
stack build
``` 

Build & run:
```
stack build --exec hn-tui
```

# Misc
hn-tui is built using the [brick](https://github.com/jtdaugherty/brick) library
