# Another Emacs Solarized Theme pair

This is an alternate Emacs color-theme version of Ethan Schoonover's
popular "Solarized" theme pair.

## Installation ##

### Via ELPA

If you have Emacs 24, which includes package.el, you can just install
the theme using the package on marmalade-repo.org.

Make sure you have something like the following in your Emacs startup
file (`~/.emacs.d/init.el`, or `~/.emacs`):

    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))

To make that take effect, either evaluate that elisp expression or restart your Emacs.

Then use `M-x package-list-packages`, select
`color-theme-sanityinc-solarized` from the list by pressing `i`, then
press `x` to execute the changes. At that point, the package will be
installed.

### Manually

Add the directory containing this README to your Emacs `load-path`,
and `require` the main file:

    (add-to-list 'load-path "/dir/containing/color-theme-sanityinc-solarized")
    (require 'color-theme-sanityinc-solarized)




## Usage ##

In recent versions of Emacs which have good built-in theme support,
you can just use `M-x customize-themes` to select themes.

In older versions, you'll need to install the venerable
`color-theme.el` package first, and require it explicitly.

Either way, the following command sequences should work in order to
activate one or other of the themes:

    M-x color-theme-sanityinc-solarized-light
    M-x color-theme-sanityinc-solarized-dark

## Background info ##

See the [Solarized page](http://ethanschoonover.com/solarized) for
more info.

Thanks to Greg Pfeil for `color-theme-solarized`, of which these
themes are a different formulation, providing a different set of faces
and a few different choices. You might want to use his version
instead.

Note that, depending on your version of Emacs, the colors in this
theme may not be rendered accurately. See, for example,
[this OS X Emacs bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8402).
