<img src="./screenshot.gif">

# What is it?

My English is not very nice, I need query translation before I insert a suitable name in my code.

This extension will insert translation as variable name when I press SPACE after Chinese.

### Installation
Then put insert-translated-name.el to your load-path.

The load-path is usually ~/elisp/.

It's set in your ~/.emacs like this:

```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'insert-translated-name)
```

### Usage.

| Command                                       | Description                                                         |
| :--------                                     | :----                                                               |
| insert-translated-name-insert                 | Insert Chinese translation with current language style              |
| insert-translated-name-insert-with-underline  | Insert Chinese translation with underline style                     |
| insert-translated-name-insert-with-camel      | Insert Chinese translation with camel style                         |
| insert-translated-name-insert-with-line       | Insert Chinese translation with line style                          |
| insert-translated-name-replace                | Replace Chinese symbol with translation with current language style |
| insert-translated-name-replace-with-underline | Replace Chinese symbol with translation with underline style        |
| insert-translated-name-replace-with-camel     | Replace Chinese symbol with translation with camel style            |
| insert-translated-name-replace-with-line      | Replace Chinese symbol with translation with line style             |
