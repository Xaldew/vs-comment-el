[![Build Status](https://travis-ci.org/Xaldew/vs-comment-el.svg?branch=master)](https://travis-ci.org/Xaldew/vs-comment-el)
[![Coverage Status](https://coveralls.io/repos/github/Xaldew/vs-comment-el/badge.svg?branch=master)](https://coveralls.io/github/Xaldew/vs-comment-el?branch=master)

# vs-comment-mode.el

 Visual Studio Comments in Emacs!

This is a small package intended to replicate the enhanced comment styles used
in the Visual Studio editor.


## Example

![](https://raw.githubusercontent.com/xaldew/vs-comment-el/master/images/vs-comment-mode.gif)


## Installing

This is currently not available on MELPA. For the time being simply download the
`vs-comment-mode.el` and it to your load-path to use the package.


## Customization

It's simple to add new customized comment annotations, simply add a new entry in
the `vs-comment-keyword-list`:

    (setq vs-comment-keyword-list (append vs-comment-keyword-list
                                          '("-" 'vs-comment-strike-through)))

The first element of the list entry should be a string with the annotation
syntax, the second element should be an Emacs `face`.
