[![Melpa Status](http://melpa.org/packages/color-theme-sanityinc-solarized-badge.svg)](http://melpa.org/#/color-theme-sanityinc-solarized)
[![Melpa Stable Status](http://stable.melpa.org/packages/color-theme-sanityinc-solarized-badge.svg)](http://stable.melpa.org/#/color-theme-sanityinc-solarized)
<a href="https://www.patreon.com/sanityinc"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>

# Another Emacs Solarized Theme pair

This is an alternate Emacs color-theme version of Ethan Schoonover's
popular "Solarized" theme pair.

## Installation ##

### Via ELPA

If you have Emacs 24, which includes package.el, you can just install
the theme using the package on [MELPA](http://melpa.org/).

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
[this OS X Emacs bug](http://debbugs.gnu.org/cgi/bugreport.cgi?bug=8402). A
customizable value `color-theme-sanityinc-solarized-rgb-is-srgb`
controls whether or not RGB color literals used by Emacs are treated
as sRGB; consider adjusting this value if the rendered colors don't
quite match those on the solarized page.

<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

