# What is it?

My English is not very nice, I need query translation before I insert a suitable name in my code.

This extension will ask me Chinese words and then insert translation as variable or function name.

### Installation
Then put insert-translated-name.el to your load-path.

The load-path is usually ~/elisp/.

It's set in your ~/.emacs like this:

```Elisp
(add-to-list 'load-path (expand-file-name "~/elisp"))
(require 'insert-translated-name)
```

### Usage.

| Command                                         | Description                                        |
| :--------                                       | :----                                              |
| insert-translated-name-with-underline           | Insert translation with underline style            |
| insert-translated-name-with-camel               | Insert translation with camel style                        |
