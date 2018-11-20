# What is css-sort.el ?
css-sort.el is an Emacs extension you can sort CSS attributables automatically.

It has the following advantages:

* You don't need to select the sorted area, it can sort all the attributes automatically with one command "css-sort".
* Sort CSS attributes exactly in accordance with [CSS sorting rules](http://alloyteam.github.io/CodeGuide/#css-declaration-order)
* Attributes starting with @ are placed at last, such as, @include attribute.
* ...

## Installation
Clone or download this repository (path of the folder is the `<path-to-css-sort>` used below).

In your `~/.emacs`, add the following two lines:
```Elisp
(add-to-list 'load-path "<path-to-css-sort>") ; add css-sort to your load-path
(require 'css-sort)
```

## Usage

M-x css-sort
